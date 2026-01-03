//! Query observers that match when an entity starts and stops
//! matching a query.
//!
//! For more information,
//! [refer to the crate documentation](crate).

use alloc::vec::Vec;
use bevy_ecs::{
    archetype::Archetype,
    component::{ComponentId, Tick},
    entity_disabling::DefaultQueryFilters,
    error::ErrorContext,
    event::{EntityComponentsTrigger, EventKey},
    lifecycle::{ADD, INSERT, REMOVE, REPLACE},
    observer::TriggerContext,
    prelude::*,
    ptr::PtrMut,
    query::{FilteredAccess, QueryFilter, WorldQuery},
    storage::Table,
    system::{BoxedSystem, RunSystemError},
    world::{DeferredWorld, unsafe_world_cell::UnsafeWorldCell},
};
use bevy_platform::collections::HashMap;
use bevy_utils::prelude::DebugName;

pub mod start;
pub mod stop;

fn query_observer_runner(
    mut world: DeferredWorld,
    observer: Entity,
    trigger_context: &TriggerContext,
    event: PtrMut,
    trigger: PtrMut,
) {
    let world = world.as_unsafe_world_cell();
    let observer_cell = world.get_entity(observer).unwrap();

    // # Safety
    //
    // In both cases, this cannot be mutably aliased.
    let observer = unsafe { observer_cell.get::<QueryObserverOf>().unwrap().0 };

    let access = {
        let mut observer_state = unsafe {
            observer_cell
                .get_mut::<QueryObserverObserverState>()
                .unwrap()
        };

        let last_trigger = world.last_trigger_id();
        if observer_state.last_trigger_id == last_trigger {
            return;
        }
        observer_state.last_trigger_id = last_trigger;

        observer_state.access
    };

    let observer_cell = world.get_entity(observer).unwrap();

    // # Safety
    //
    // Because this state is private, no one else can get useful mutable access to
    // it, and it can't be dropped until after this function completes.
    let mut query_state = unsafe { observer_cell.get_mut::<QueryObserverState>().unwrap() };

    // # Safety
    //
    // Since we only construct this runner for lifecycle events, the trigger
    // _must_ be `EntityComponentsTrigger`. Note that this is especially unsafe
    // as well, as noted in the normal observer's safety comments and
    // <https://github.com/bevyengine/bevy/pull/20731#discussion_r2311907935>.
    //
    // Since we're explicitly not exposing this trigger, our safety constraints
    // aren't as dire.
    let trigger: &mut EntityComponentsTrigger<'_> = unsafe { trigger.deref_mut() };

    let (target, deferred) = match trigger_context.event_key {
        ADD => {
            // # Safety
            //
            // According to the event key, this must be an `Add` event.
            let event: &mut Add = unsafe { event.deref_mut() };
            (
                event.entity,
                matches!(access.add, LifecycleAccess::Deferred),
            )
        }
        INSERT => {
            // # Safety
            //
            // According to the event key, this must be an `Insert` event.
            let event: &mut Insert = unsafe { event.deref_mut() };
            (
                event.entity,
                matches!(access.insert, LifecycleAccess::Deferred),
            )
        }
        REPLACE => {
            // # Safety
            //
            // According to the event key, this must be a `Replace` event.
            let event: &mut Replace = unsafe { event.deref_mut() };
            (
                event.entity,
                matches!(access.replace, LifecycleAccess::Deferred),
            )
        }
        REMOVE => {
            // # Safety
            //
            // According to the event key, this must be a `Remove` event.
            let event: &mut Remove = unsafe { event.deref_mut() };
            (
                event.entity,
                matches!(access.remove, LifecycleAccess::Deferred),
            )
        }
        _ => panic!("triggered query observer with unexpected event key"),
    };

    let target_entity = world.get_entity(target).unwrap();
    let should_run = (query_state.evaluator)(
        world,
        target_entity.archetype(),
        query_state.kind,
        &trigger.components,
    );

    if should_run {
        let last_key = query_state.last_key;
        query_state.last_key = Some(trigger_context.event_key);

        let last_trigger = world.last_trigger_id();
        let is_adjacent_tick = query_state.last_trigger_id + 1 == last_trigger;
        query_state.last_trigger_id = last_trigger;

        // A query observer with either add and insert or remove and replace
        // observers could erroneously trigger twice for the same "moment"
        // without this check.
        //
        // Since both add and insert and replace and remove are necessarily
        // adjacent, this cannot accidentally filter out legitimate events.
        if is_adjacent_tick {
            match last_key {
                Some(ADD)
                    if query_state.is_add_and_insert && trigger_context.event_key == INSERT =>
                {
                    // skip evaluation
                    return;
                }
                Some(REPLACE)
                    if query_state.is_remove_and_replace && trigger_context.event_key == REMOVE =>
                {
                    // also skip
                    return;
                }
                _ => {}
            }
        }
    }

    if should_run {
        if !deferred {
            let system = query_state.system.as_mut();
            unsafe {
                if let Err(RunSystemError::Failed(err)) = system
                    .validate_param_unsafe(world)
                    .map_err(From::from)
                    .and_then(|_| system.run_unsafe(target, world))
                {
                    let handler = world.default_error_handler();
                    handler(
                        err,
                        ErrorContext::Observer {
                            name: system.name(),
                            last_run: system.get_last_run(),
                        },
                    );
                }

                system.queue_deferred(world.into_deferred());
            }
        } else {
            unsafe {
                world
                    .into_deferred()
                    .commands()
                    .queue(move |world: &mut World| -> Result {
                        let mut state = world.get_entity_mut(observer)?;
                        let mut state = state
                            .take::<QueryObserverState>()
                            .ok_or("Expected `QueryObserverState`")?;

                        let result = state.system.run_without_applying_deferred(target, world);
                        world.entity_mut(observer).insert(state);
                        world.flush();

                        match result {
                            Err(RunSystemError::Failed(e)) => Err(e),
                            _ => Ok(()),
                        }
                    });
            }
        }
    }
}

type Evaluator = fn(UnsafeWorldCell, &Archetype, QueryObserverKind, &[ComponentId]) -> bool;

#[derive(Component)]
struct QueryObserverState {
    evaluator: Evaluator,
    kind: QueryObserverKind,
    system: BoxedSystem<In<Entity>, ()>,
    last_key: Option<EventKey>,
    last_trigger_id: u32,
    is_add_and_insert: bool,
    is_remove_and_replace: bool,
}

/// A relationship linking an [`Observer`] of a specific query term
/// to the main [`QueryObserer`] entity.
#[derive(Component, Debug)]
#[relationship(relationship_target = QueryObservers)]
pub struct QueryObserverOf(pub Entity);

/// The set of all observers for each term in a query observer.
#[derive(Component, Debug)]
#[relationship_target(relationship = QueryObserverOf, linked_spawn)]
pub struct QueryObservers(Vec<Entity>);

/// A convenience trait for dynamically spawning a [`QueryObserver`].
pub trait SpawnQueryObserver {
    /// Spawns a new [`QueryObserver`], returning its [`Entity`].
    fn spawn_query_observer(&mut self, observer: QueryObserver) -> Entity;
}

impl SpawnQueryObserver for World {
    fn spawn_query_observer(&mut self, observer: QueryObserver) -> Entity {
        let target = self.spawn_empty().id();
        observer.insert_into(target, self);
        target
    }
}

impl SpawnQueryObserver for Commands<'_, '_> {
    fn spawn_query_observer(&mut self, observer: QueryObserver) -> Entity {
        let target = self.spawn_empty().id();
        self.queue(move |world: &mut World| observer.insert_into(target, world));
        target
    }
}

/// A convenience trait for dynamically inserting a [`QueryObserver`].
pub trait InsertQueryObserver {
    /// Insert a [`QueryObserver`] into the entity.
    fn insert_query_observer(&mut self, observer: QueryObserver) -> &mut Self;
}

impl InsertQueryObserver for EntityCommands<'_> {
    fn insert_query_observer(&mut self, observer: QueryObserver) -> &mut Self {
        let id = self.id();
        self.commands()
            .queue(move |world: &mut World| observer.insert_into(id, world));
        self
    }
}

/// An extension trait for manually triggering a [`QueryObserver`].
///
/// This can be used to build a simple reactivity framework in terms
/// of Bevy's lifecycle events. Triggering a query observer manually
/// allows it to respond to specific entities even if its lifecycle events
/// occurred before the observer was spawned.
pub trait TriggerQueryObserver {
    /// Trigger a [`QueryObserver`] (`observer`) with a given `target`.
    ///
    /// If the target doesn't fulfill the query, the system will not run.
    fn trigger_query_observer(&mut self, observer: Entity, target: Entity) -> &mut Self;
}

impl TriggerQueryObserver for World {
    fn trigger_query_observer(&mut self, observer: Entity, target: Entity) -> &mut Self {
        let world = self.as_unsafe_world_cell();

        let observer_cell = world.get_entity(observer).unwrap();

        // # Safety
        //
        // Because this state is private, no one else can get useful mutable access to
        // it, and it can't be dropped until after this function completes.
        let mut query_state = unsafe { observer_cell.get_mut::<QueryObserverState>().unwrap() };

        let system = query_state.system.as_mut();
        unsafe {
            if let Err(RunSystemError::Failed(err)) = system
                .validate_param_unsafe(world)
                .map_err(From::from)
                .and_then(|_| system.run_unsafe(target, world))
            {
                let handler = world.default_error_handler();
                handler(
                    err,
                    ErrorContext::Observer {
                        name: system.name(),
                        last_run: system.get_last_run(),
                    },
                );
            }

            system.queue_deferred(world.into_deferred());
        }

        self
    }
}

/// An observer that triggers
/// when an entity starts or stops matching a query.
/// [`QueryObserver`] also triggers when a matching entity's data
/// changes according to lifecycle events.
pub struct QueryObserver {
    system: BoxedSystem<In<Entity>, ()>,
    entities: Option<Vec<Entity>>,
    get_access: fn(&mut World, QueryObserverKind) -> Access,
    kind: QueryObserverKind,
    evaluator: Evaluator,
}

impl QueryObserver {
    /// Observes the given [`Entity`] (in addition to any entity already being observed).
    pub fn watch_entity(&mut self, entity: Entity) {
        self.entities.get_or_insert_default().push(entity);
    }

    /// Observes the given [`Entity`] (in addition to any entity already being observed).
    pub fn with_entity(mut self, entity: Entity) -> Self {
        self.watch_entity(entity);
        self
    }

    /// Observes each [`Entity`] in the iterator (in addition to any entity already being observed).
    pub fn watch_entities<I>(&mut self, entities: I)
    where
        I: IntoIterator<Item = Entity>,
    {
        self.entities.get_or_insert_default().extend(entities)
    }

    /// Observes each [`Entity`] in the iterator (in addition to any entity already being observed).
    pub fn with_entities<I>(mut self, entities: I) -> Self
    where
        I: IntoIterator<Item = Entity>,
    {
        self.watch_entities(entities);
        self
    }

    /// Returns the name of the [`QueryObserver`]'s system.
    pub fn system_name(&self) -> DebugName {
        self.system.name()
    }

    /// Insert a [`QueryObserver`] into the given [`Entity`].
    ///
    /// Unlike [`Observer`], [`QueryObserver`] is not itself a [`Component`].
    /// This method inserts the [`QueryObserver`]'s private state into `state_entity`,
    /// along with related entities containing custom [`Observer`]s for each unique
    /// component in the query.
    ///
    /// To properly clean up a [`QueryObserver`] entity, all entities related by [`QueryObservers`]
    /// should be despawned and the entity should be cleared.
    pub fn insert_into(self, state_entity: Entity, world: &mut World) {
        let QueryObserver {
            mut system,
            entities,
            get_access,
            kind,
            evaluator,
        } = self;

        system.initialize(world);

        let access = get_access(world, kind);
        let state = QueryObserverState {
            evaluator,
            system,
            kind,
            last_key: None,
            last_trigger_id: 0,
            is_add_and_insert: access.is_add_and_insert(),
            is_remove_and_replace: access.is_remove_and_replace(),
        };

        world.entity_mut(state_entity).insert(state);

        let mut observer_sets = HashMap::<_, Vec<ComponentId>>::default();
        for component in access.set {
            let set = observer_sets.entry(component.access).or_default();
            if !set.contains(&component.id) {
                set.push(component.id);
            }
        }

        for (access, ids) in observer_sets.into_iter() {
            let mut observer = Observer::with_dynamic_runner(query_observer_runner);
            if let Some(entities) = &entities {
                observer.watch_entities(entities.iter().copied());
            }

            for key in access.event_keys() {
                // # Safety
                //
                // Our default runner knows how to handler these events.
                observer = unsafe { observer.with_event_key(key) };
            }

            for component in ids {
                observer = observer.with_component(component);
            }

            world.spawn((
                QueryObserverOf(state_entity),
                QueryObserverObserverState {
                    access,
                    last_trigger_id: 0,
                },
                observer,
            ));
        }
    }
}

impl core::fmt::Debug for QueryObserver {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("QueryObserver")
            .field("system", &self.system)
            .field("kind", &self.kind)
            .field("entities", &self.entities)
            .finish_non_exhaustive()
    }
}

/// Describes whether a query term requires access, and
/// whether that access should be deferred (for example,
/// allowing a component to be removed before evaluation).
#[derive(PartialEq, Eq, Default, Hash, Clone, Copy, Debug)]
pub enum LifecycleAccess {
    /// No access required.
    #[default]
    None,
    /// Immediate access is required.
    Immediate,
    /// Deferred access is required, allowing
    /// the effects of a lifecycle event (like removing a component)
    /// to occur before running.
    Deferred,
}

impl LifecycleAccess {
    fn set_immediate(&mut self) {
        *self = Self::Immediate;
    }

    fn set_deferred(&mut self) {
        if !matches!(self, Self::Immediate) {
            *self = Self::Deferred;
        }
    }
}

/// Distinguishes [`Start`][start::Start] and [`Stop`][stop::Stop]
/// query observers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueryObserverKind {
    /// A [`Start`][start::Start] observer.
    Start,
    /// A [`Stop`][stop::Stop] observer.
    Stop,
}

/// Provides a simplified set of component access information
/// for query observers.
pub trait QueryObserverAccess {
    /// Report a query term's component access given the [`QueryObserverKind`].
    fn report_access(world: &mut World, kind: QueryObserverKind, access: &mut Access);

    /// Evaluate whether an archetype matches the query term.
    ///
    /// Certain queries, like `Start<(), Without<Name>>` require a method
    /// like this in order to produce
    /// [approximately correct behavior](crate#limitations).
    fn evaluate_archetype(
        world: UnsafeWorldCell,
        archetype: &Archetype,
        kind: QueryObserverKind,
        triggered_by: &[ComponentId],
    ) -> bool {
        {
            let _ = world;
            let _ = archetype;
            let _ = kind;
            let _ = triggered_by;
        }

        true
    }
}

impl<T: Component> QueryObserverAccess for &'static T {
    fn report_access(world: &mut World, kind: QueryObserverKind, access: &mut Access) {
        let component = world.register_component::<T>();
        let entry = access.entry(component);

        match kind {
            QueryObserverKind::Start => {
                entry.insert.set_immediate();
            }
            QueryObserverKind::Stop => {
                entry.replace.set_immediate();
            }
        }
    }

    fn evaluate_archetype(
        world: UnsafeWorldCell,
        archetype: &Archetype,
        _kind: QueryObserverKind,
        _triggered_by: &[ComponentId],
    ) -> bool {
        archetype.contains(world.components().component_id::<T>().unwrap())
    }
}

impl<T: Component> QueryObserverAccess for &'static mut T {
    fn report_access(world: &mut World, kind: QueryObserverKind, access: &mut Access) {
        let component = world.register_component::<T>();
        let entry = access.entry(component);

        match kind {
            QueryObserverKind::Start => {
                entry.insert.set_immediate();
            }
            QueryObserverKind::Stop => {
                entry.replace.set_immediate();
            }
        }
    }

    fn evaluate_archetype(
        world: UnsafeWorldCell,
        archetype: &Archetype,
        _kind: QueryObserverKind,
        _triggered_by: &[ComponentId],
    ) -> bool {
        archetype.contains(world.components().component_id::<T>().unwrap())
    }
}

impl<T: Component> QueryObserverAccess for Option<&'static T> {
    fn report_access(world: &mut World, kind: QueryObserverKind, access: &mut Access) {
        let component = world.register_component::<T>();
        let entry = access.entry(component);

        match kind {
            QueryObserverKind::Start => {
                entry.insert.set_immediate();
                entry.remove.set_deferred();
            }
            QueryObserverKind::Stop => {
                // TODO: can't express before add
                // entry.add.set_immediate();
                entry.replace.set_immediate();
            }
        }
    }
}

impl<T: Component> QueryObserverAccess for Option<&'static mut T> {
    fn report_access(world: &mut World, kind: QueryObserverKind, access: &mut Access) {
        let component = world.register_component::<T>();
        let entry = access.entry(component);

        match kind {
            QueryObserverKind::Start => {
                entry.insert.set_immediate();
                entry.remove.set_deferred();
            }
            QueryObserverKind::Stop => {
                // TODO: can't express before add
                // entry.add.set_immediate();
                entry.replace.set_immediate();
            }
        }
    }
}

impl<T: Component> QueryObserverAccess for Has<T> {
    fn report_access(world: &mut World, kind: QueryObserverKind, access: &mut Access) {
        let component = world.register_component::<T>();
        let entry = access.entry(component);

        match kind {
            QueryObserverKind::Start => {
                entry.add.set_immediate();
                entry.remove.set_deferred();
            }
            QueryObserverKind::Stop => {
                // TODO: can't express before add
                // entry.add.set_immediate();
                entry.remove.set_immediate();
            }
        }
    }
}

struct DefaultFilterBanisher;

// # Safety
//
// This is a trivial implementation
unsafe impl QueryFilter for DefaultFilterBanisher {
    const IS_ARCHETYPAL: bool = true;

    unsafe fn filter_fetch(
        _state: &Self::State,
        _fetch: &mut Self::Fetch<'_>,
        _entity: Entity,
        _table_row: bevy_ecs::storage::TableRow,
    ) -> bool {
        true
    }
}

/// SAFETY:
/// `update_component_access` does not add any accesses.
/// This is sound because [`QueryFilter::filter_fetch`] does not access any components.
/// This is sound because it doesn't affect the query
unsafe impl WorldQuery for DefaultFilterBanisher {
    type Fetch<'w> = ();
    type State = Vec<ComponentId>;

    fn shrink_fetch<'wlong: 'wshort, 'wshort>(_: Self::Fetch<'wlong>) -> Self::Fetch<'wshort> {}

    #[inline]
    unsafe fn init_fetch(_: UnsafeWorldCell, _: &Self::State, _: Tick, _: Tick) {}

    // Even if the components are sparse, this implementation doesn't do anything with it
    const IS_DENSE: bool = true;

    #[inline]
    unsafe fn set_archetype(_: &mut (), _: &Self::State, _: &Archetype, _: &Table) {}

    #[inline]
    unsafe fn set_table(_: &mut (), _: &Self::State, _: &Table) {}

    #[inline]
    fn update_component_access(state: &Self::State, access: &mut FilteredAccess) {
        for id in state {
            access.access_mut().add_archetypal(*id);
        }
    }

    fn init_state(world: &mut World) -> Self::State {
        world
            .resource::<DefaultQueryFilters>()
            .disabling_ids()
            .collect()
    }

    fn get_state(_: &bevy_ecs::component::Components) -> Option<Self::State> {
        None
    }

    fn matches_component_set(_: &Self::State, _: &impl Fn(ComponentId) -> bool) -> bool {
        // Allow<T> always matches
        true
    }
}

impl QueryObserverAccess for DefaultFilterBanisher {
    fn report_access(world: &mut World, kind: QueryObserverKind, access: &mut Access) {
        for id in world.resource::<DefaultQueryFilters>().disabling_ids() {
            if !access.contains(id) {
                match kind {
                    QueryObserverKind::Start => {
                        access.entry(id).remove.set_deferred();
                    }
                    QueryObserverKind::Stop => {
                        access.entry(id).add.set_immediate();
                    }
                }
            }
        }
    }

    fn evaluate_archetype(
        world: UnsafeWorldCell,
        archetype: &Archetype,
        _: QueryObserverKind,
        triggered_by: &[ComponentId],
    ) -> bool {
        // # Safety
        //
        // The world must not be mutable accessed within this trait.
        let filter = unsafe { world.get_resource::<DefaultQueryFilters>().unwrap() };
        for id in filter.disabling_ids() {
            if archetype.contains(id) && !triggered_by.contains(&id) {
                return false;
            }
        }

        true
    }
}

impl QueryObserverAccess for Entity {
    fn report_access(_world: &mut World, _kind: QueryObserverKind, _access: &mut Access) {}
}

impl QueryObserverAccess for () {
    fn report_access(_world: &mut World, _kind: QueryObserverKind, _access: &mut Access) {}
}

impl<T: Component> QueryObserverAccess for With<T> {
    fn report_access(world: &mut World, kind: QueryObserverKind, access: &mut Access) {
        let component = world.register_component::<T>();
        let entry = access.entry(component);

        match kind {
            QueryObserverKind::Start => {
                entry.add.set_immediate();
            }
            QueryObserverKind::Stop => {
                entry.remove.set_immediate();
            }
        }
    }

    fn evaluate_archetype(
        world: UnsafeWorldCell,
        archetype: &Archetype,
        _: QueryObserverKind,
        _: &[ComponentId],
    ) -> bool {
        let id = world.components().component_id::<T>().unwrap();
        archetype.contains(id)
    }
}

impl<T: Component> QueryObserverAccess for Without<T> {
    fn report_access(world: &mut World, kind: QueryObserverKind, access: &mut Access) {
        let component = world.register_component::<T>();
        let entry = access.entry(component);

        match kind {
            QueryObserverKind::Start => {
                entry.remove.set_deferred();
            }
            QueryObserverKind::Stop => {
                // TODO: can't express before add
                entry.add.set_immediate();
            }
        }
    }

    fn evaluate_archetype(
        world: UnsafeWorldCell,
        archetype: &Archetype,
        _kind: QueryObserverKind,
        triggered_by: &[ComponentId],
    ) -> bool {
        let id = world.components().component_id::<T>().unwrap();

        if triggered_by.contains(&id) {
            true
        } else {
            !archetype.contains(id)
        }
    }
}

macro_rules! query_observer_data {
    ($($ty:ident),*) => {
        impl<$($ty),*> QueryObserverAccess for ($($ty,)*)
        where $($ty: QueryObserverAccess),*
        {
            fn report_access(
                world: &mut World,
                kind: QueryObserverKind,
                access: &mut Access,
            ) {
                $(
                    <$ty as QueryObserverAccess>::report_access(world, kind, access);
                )*
            }

            fn evaluate_archetype(
                world: UnsafeWorldCell,
                archetype: &Archetype,
                kind: QueryObserverKind,
                triggered_by: &[ComponentId],
            ) -> bool {
                $(
                    <$ty as QueryObserverAccess>::evaluate_archetype(world, archetype, kind, triggered_by)
                )&&*
            }
        }
    };
}

variadics_please::all_tuples!(query_observer_data, 1, 15, T);

/// Describes which lifecycle events, if any, a query observer
/// term needs to respond to.
#[derive(PartialEq, Eq, Default, Hash, Clone, Copy, Debug)]
pub struct QueryAccess {
    /// Access for the add lifecycle event.
    pub add: LifecycleAccess,
    /// Access for the insert lifecycle event.
    pub insert: LifecycleAccess,
    /// Access for the replace lifecycle event.
    pub replace: LifecycleAccess,
    /// Access for the remove lifecycle event.
    pub remove: LifecycleAccess,
}

impl QueryAccess {
    fn event_keys(&self) -> impl Iterator<Item = EventKey> {
        let add = (self.add != LifecycleAccess::None).then_some(ADD);
        let insert = (self.insert != LifecycleAccess::None).then_some(INSERT);
        let replace = (self.replace != LifecycleAccess::None).then_some(REPLACE);
        let remove = (self.remove != LifecycleAccess::None).then_some(REMOVE);

        add.into_iter().chain(insert).chain(replace).chain(remove)
    }
}

#[derive(Component)]
struct QueryObserverObserverState {
    access: QueryAccess,
    last_trigger_id: u32,
}

#[derive(Debug)]
struct ComponentAccess {
    id: ComponentId,
    access: QueryAccess,
}

/// The full set of components a query observer needs
/// to access.
#[derive(Default, Debug)]
pub struct Access {
    set: Vec<ComponentAccess>,
}

impl Access {
    fn is_add_and_insert(&self) -> bool {
        let mut add = false;
        let mut insert = false;

        for access in self.set.iter() {
            if !matches!(access.access.add, LifecycleAccess::None) {
                add = true;
            }

            if !matches!(access.access.insert, LifecycleAccess::None) {
                insert = true;
            }
        }

        add && insert
    }

    fn is_remove_and_replace(&self) -> bool {
        let mut remove = false;
        let mut replace = false;

        for access in self.set.iter() {
            if !matches!(access.access.remove, LifecycleAccess::None) {
                remove = true;
            }

            if !matches!(access.access.replace, LifecycleAccess::None) {
                replace = true;
            }
        }

        remove && replace
    }
}

impl Access {
    /// Initialize or fetch an entry for `id`.
    pub fn entry(&mut self, id: ComponentId) -> &mut QueryAccess {
        let position = self.set.iter().position(|a| a.id == id).unwrap_or_else(|| {
            self.set.push(ComponentAccess {
                id,
                access: Default::default(),
            });

            self.set.len() - 1
        });

        &mut self.set[position].access
    }

    /// Returns `true` if the set contains the component `id`.
    pub fn contains(&self, id: ComponentId) -> bool {
        self.set.iter().any(|a| a.id == id)
    }
}

/// A marker type for infallible query observers.
#[derive(Debug)]
pub struct Infallible;

//! Implementations for [`Stop`] query observers.

use alloc::{boxed::Box, vec::Vec};
use bevy_ecs::{
    archetype::Archetype,
    prelude::*,
    query::{QueryData, QueryFilter},
    system::{SystemParamValidationError, SystemState},
    world::unsafe_world_cell::UnsafeWorldCell,
};
use bevy_utils::prelude::DebugName;
use core::marker::PhantomData;

use crate::{
    QueryObserver,
    observer::{AllowAll, Infallible, QueryObserverAccess, SpawnQueryObserver},
};

/// A [`SystemInput`] used by a query observer.
///
/// [`Stop`] triggers when an entity stops matching an archetypal query.
/// It contains the query's data, fetched from the triggering entity.
///
/// If you need access to the entity that triggered a query observer,
/// you can add [`Entity`] to the query as with a normal [`Query`].
///
/// ```
/// # use bevy::prelude::*;
/// # use bevy_query_observer::*;
/// fn named_entity(data: Stop<(Entity, &Name)>) {
///     let (entity, name) = data.into_inner();
///     info!("entity {entity:?} is no longer named {name}");
/// }
/// ```
///
/// Because query observers are implemented in terms of component lifecycle
/// events, any query filters must be archetypal. Filters like [`Changed`] or
/// [`Added`] are not supported.
///
/// Note that the filter will not necessarily be used when fetching an entity.
/// Some filters, like `Without<C>`, necessitate running a query observer even
/// when the target entity has `C`. Consequently, a query observer system may
/// appear to be more restrictive than necessary, and some invariants may not
/// hold depending on the query terms.
pub struct Stop<'w, 's, D: QueryData, F: QueryFilter = ()> {
    data: D::Item<'w, 's>,
    filter: PhantomData<fn() -> F>,
}

impl<D: QueryData, F: QueryFilter> SystemInput for Stop<'_, '_, D, F> {
    type Param<'i> = Stop<'i, 'i, D, F>;
    type Inner<'i> = D::Item<'i, 'i>;

    fn wrap(this: Self::Inner<'_>) -> Self::Param<'_> {
        Stop {
            data: this,
            filter: PhantomData,
        }
    }
}

impl<'w, 's, D: QueryData, F: QueryFilter> Stop<'w, 's, D, F> {
    /// Consume the [`Stop`], returning the query data.
    ///
    /// ```
    /// # use bevy::prelude::*;
    /// # use bevy_query_observer::*;
    /// fn named_entity(data: Stop<(Entity, &Name)>) {
    ///     let (entity, name) = data.into_inner();
    ///     info!("entity {entity:?} is no longer named {name}");
    /// }
    /// ```
    pub fn into_inner(self) -> D::Item<'w, 's> {
        self.data
    }
}

impl<'w, 's, D: QueryData, F: QueryFilter> core::ops::Deref for Stop<'w, 's, D, F> {
    type Target = D::Item<'w, 's>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<'w, 's, D: QueryData, F: QueryFilter> core::ops::DerefMut for Stop<'w, 's, D, F> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<'w, 's, D: QueryData, F: QueryFilter> core::fmt::Debug for Stop<'w, 's, D, F>
where
    D::Item<'w, 's>: core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Stop")
            .field("data", &self.data)
            .finish_non_exhaustive()
    }
}

/// Implemented for [`System`]s with [`Stop`] as the first argument.
pub trait StopSystem<
    D: QueryData + QueryObserverAccess,
    F: QueryFilter + QueryObserverAccess,
    Out = Result,
>: System<In = Stop<'static, 'static, D, F>, Out = Out> + Send + 'static
{
}

impl<D: QueryData + QueryObserverAccess, F: QueryFilter + QueryObserverAccess, Out, T>
    StopSystem<D, F, Out> for T
where
    T: System<In = Stop<'static, 'static, D, F>, Out = Out> + Send + 'static,
{
}

/// Implemented for systems that convert into [`StopSystem`].
pub trait IntoStopSystem<
    D: QueryData + QueryObserverAccess,
    F: QueryFilter + QueryObserverAccess,
    M,
    Out = (),
>: Send + 'static
{
    /// The type of [`System`] that this type converts into.
    type System: StopSystem<D, F, Out>;

    /// Consumes this value and converts it into its corresponding [`System`].
    fn into_system(this: Self) -> Self::System;
}

impl<D, F, M, S, Out> IntoStopSystem<D, F, (Infallible, M), Out> for S
where
    S: IntoSystem<Stop<'static, 'static, D, F>, Out, M> + Send + 'static,
    S::System: StopSystem<D, F, Out>,
    D: QueryData + QueryObserverAccess + 'static,
    F: QueryFilter + QueryObserverAccess + 'static,
{
    type System = S::System;

    fn into_system(this: Self) -> Self::System {
        IntoSystem::into_system(this)
    }
}

/// A convenience trait for adding [`Start`] query observers.
pub trait AddStopObserver {
    /// Add a global [`Stop`] query observer.
    ///
    /// ```
    /// # use bevy::prelude::*;
    /// # use bevy_query_observer::*;
    /// fn plugin(app: &mut App) {
    ///     app.add_stop_observer(named_entity);
    /// }
    ///
    /// fn named_entity(data: Stop<(Entity, &Name)>) {
    ///     let (entity, name) = data.into_inner();
    ///     info!("entity {entity:?} is no longer named {name}");
    /// }
    /// ```
    ///
    /// # Panics
    ///
    /// Panics if the given system is exclusive.
    fn add_stop_observer<S, D, F, M>(&mut self, system: S) -> &mut Self
    where
        S: IntoStopSystem<D, F, M, ()>,
        D: QueryData + QueryObserverAccess + 'static,
        F: QueryFilter + QueryObserverAccess + 'static;
}

impl AddStopObserver for bevy_app::App {
    fn add_stop_observer<S, D, F, M>(&mut self, system: S) -> &mut Self
    where
        S: IntoStopSystem<D, F, M, ()>,
        D: QueryData + QueryObserverAccess + 'static,
        F: QueryFilter + QueryObserverAccess + 'static,
    {
        self.world_mut()
            .spawn_query_observer(QueryObserver::stop(system));
        self
    }
}

impl super::QueryObserver {
    /// Construct a [`QueryObserver`] that triggers when an entity
    /// stops matching the query. The first argument of the system
    /// must be [`Stop`].
    ///
    /// ```
    /// # use bevy::prelude::*;
    /// # use bevy_query_observer::*;
    /// # fn target_entity(mut commands: Commands) {
    /// fn named_child(data: Stop<(&Name, &ChildOf)>) {
    ///     let (name, parent) = data.into_inner();
    ///     info!("{name} is no longer a named child of {parent:?}");
    /// }
    ///
    /// let query_observer = QueryObserver::stop(named_child);
    /// commands.spawn_query_observer(query_observer);
    /// # }
    /// ```
    ///
    /// Unless specific targets are added, this observer responds to
    /// all entities.
    ///
    /// # Panics
    ///
    /// Panics if the given system is exclusive.
    pub fn stop<S, D, F, M>(observer: S) -> Self
    where
        S: IntoStopSystem<D, F, M, ()>,
        D: QueryData + QueryObserverAccess + 'static,
        F: QueryFilter + QueryObserverAccess + 'static,
    {
        let evaluator =
            |world: UnsafeWorldCell<'_>, archetype: &Archetype, kind, triggered_by: &[_]| {
                D::evaluate_archetype(world, archetype, kind, triggered_by)
                    && F::evaluate_archetype(world, archetype, kind, triggered_by)
                    && AllowAll::evaluate_archetype(world, archetype, kind, triggered_by)
            };

        let kind = super::QueryObserverKind::Stop;
        let access_getter = |world: &mut World, kind: super::QueryObserverKind| {
            let mut access = super::Access::default();
            D::report_access(world, kind, &mut access);
            F::report_access(world, kind, &mut access);
            AllowAll::report_access(world, kind, &mut access);
            access
        };

        let system = InfallibleStopSystem::new(IntoStopSystem::into_system(observer));
        assert!(
            !system.is_exclusive(),
            concat!(
                "Exclusive system `{}` may not be used as observer.\n",
                "Instead of `&mut World`, use either `DeferredWorld` if you do not need structural changes, or `Commands` if you do."
            ),
            system.name()
        );

        let system = Box::new(system);

        super::QueryObserver {
            system,
            entities: None,
            get_access: access_getter,
            kind,
            evaluator,
        }
    }
}

struct InfallibleStopSystem<S, D, F>
where
    D: QueryData + QueryObserverAccess + 'static,
    F: QueryFilter + QueryObserverAccess + 'static,
{
    input_state: Option<SystemState<Query<'static, 'static, D, AllowAll>>>,
    marker: PhantomData<fn() -> F>,
    system: S,
}

impl<D, F> InfallibleStopSystem<(), D, F>
where
    D: QueryData + QueryObserverAccess + 'static,
    F: QueryFilter + QueryObserverAccess + 'static,
{
    pub fn new<S>(system: S) -> InfallibleStopSystem<S, D, F>
    where
        S: System<In = Stop<'static, 'static, D, F>>,
    {
        InfallibleStopSystem {
            input_state: None,
            marker: PhantomData,
            system: IntoStopSystem::into_system(system),
        }
    }
}

impl<S, D, F> InfallibleStopSystem<S, D, F>
where
    S: System<In = Stop<'static, 'static, D, F>>,
    D: QueryData + QueryObserverAccess + 'static,
    F: QueryFilter + QueryObserverAccess + 'static,
{
    fn input_state(&self) -> &SystemState<Query<'static, 'static, D, AllowAll>> {
        self.input_state
            .as_ref()
            .expect("system must be initialized")
    }
}

impl<S, D, F> System for InfallibleStopSystem<S, D, F>
where
    S: System<In = Stop<'static, 'static, D, F>>,
    D: QueryData + QueryObserverAccess + 'static,
    F: QueryFilter + QueryObserverAccess + 'static,
{
    type In = In<Entity>;
    type Out = ();

    fn apply_deferred(&mut self, world: &mut World) {
        self.system.apply_deferred(world)
    }

    fn check_change_tick(&mut self, check: bevy_ecs::component::CheckChangeTicks) {
        self.system.check_change_tick(check);
    }

    fn default_system_sets(&self) -> Vec<bevy_ecs::schedule::InternedSystemSet> {
        self.system.default_system_sets()
    }

    fn flags(&self) -> bevy_ecs::system::SystemStateFlags {
        self.system.flags()
    }

    fn get_last_run(&self) -> bevy_ecs::component::Tick {
        self.system.get_last_run()
    }

    fn has_deferred(&self) -> bool {
        self.system.has_deferred() || self.input_state().meta().has_deferred()
    }

    fn initialize(&mut self, world: &mut World) -> bevy_ecs::query::FilteredAccessSet {
        let input_state = SystemState::<Query<D, AllowAll>>::new(world);

        let param_state = input_state.param_state();
        let component_access = param_state.component_access();

        let mut set = self.system.initialize(world);

        if !set.get_conflicts_single(component_access).is_empty() {
            panic!(
                "`Stop` access conflicts with one or more system parameters in `{}`",
                self.system.name()
            );
        }

        set.add(component_access.clone());

        self.input_state = Some(input_state);

        set
    }

    fn is_exclusive(&self) -> bool {
        self.system.is_exclusive()
    }

    fn is_send(&self) -> bool {
        self.system.is_send() && self.input_state().meta().is_send()
    }

    fn name(&self) -> DebugName {
        self.system.name()
    }

    fn queue_deferred(&mut self, world: bevy_ecs::world::DeferredWorld) {
        self.system.queue_deferred(world);
    }

    unsafe fn run_unsafe(
        &mut self,
        entity: SystemIn<'_, Self>,
        world: bevy_ecs::world::unsafe_world_cell::UnsafeWorldCell,
    ) -> core::result::Result<Self::Out, bevy_ecs::system::RunSystemError> {
        let state = self
            .input_state
            .as_mut()
            .expect("system state must be initialized");

        let mut input = unsafe { state.get_unchecked(world) };
        let input = input.get_mut(entity).map_err(|_| {
            bevy_ecs::system::RunSystemError::Skipped(SystemParamValidationError::new::<S::In>(
                true,
                "",
                "the target entity doesn't match",
            ))
        })?;
        unsafe {
            self.system.run_unsafe(input, world)?;
        }

        Ok(())
    }

    fn set_last_run(&mut self, last_run: bevy_ecs::component::Tick) {
        self.system.set_last_run(last_run);
    }

    fn type_id(&self) -> core::any::TypeId {
        core::any::TypeId::of::<Self>()
    }

    unsafe fn validate_param_unsafe(
        &mut self,
        world: bevy_ecs::world::unsafe_world_cell::UnsafeWorldCell,
    ) -> core::result::Result<(), bevy_ecs::system::SystemParamValidationError> {
        let state = self
            .input_state
            .as_mut()
            .expect("system state must be initialized");
        unsafe { SystemState::<_>::validate_param(state, world)? };
        unsafe { self.system.validate_param_unsafe(world) }
    }
}

use crate::{
    QueryObserver,
    observer::{DefaultFilterBanisher, QueryObserverAccess, SpawnQueryObserver},
};
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

pub struct Start<'w, 's, D: QueryData, F: QueryFilter = ()> {
    data: D::Item<'w, 's>,
    filter: PhantomData<fn() -> F>,
}

impl<D: QueryData, F: QueryFilter> SystemInput for Start<'_, '_, D, F> {
    type Param<'i> = Start<'i, 'i, D, F>;
    type Inner<'i> = D::Item<'i, 'i>;

    fn wrap(this: Self::Inner<'_>) -> Self::Param<'_> {
        Start {
            data: this,
            filter: PhantomData,
        }
    }
}

impl<'w, 's, D: QueryData, F: QueryFilter> Start<'w, 's, D, F> {
    pub fn into_inner(self) -> D::Item<'w, 's> {
        self.data
    }
}

impl<'w, 's, D: QueryData, F: QueryFilter> core::ops::Deref for Start<'w, 's, D, F> {
    type Target = D::Item<'w, 's>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<'w, 's, D: QueryData, F: QueryFilter> core::ops::DerefMut for Start<'w, 's, D, F> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

pub trait StartSystem<
    D: QueryData + QueryObserverAccess,
    F: QueryFilter + QueryObserverAccess,
    Out = Result,
>: System<In = Start<'static, 'static, D, F>, Out = Out> + Send + 'static
{
}

impl<D: QueryData + QueryObserverAccess, F: QueryFilter + QueryObserverAccess, Out, T>
    StartSystem<D, F, Out> for T
where
    T: System<In = Start<'static, 'static, D, F>, Out = Out> + Send + 'static,
{
}

pub trait IntoStartSystem<
    D: QueryData + QueryObserverAccess,
    F: QueryFilter + QueryObserverAccess,
    M,
    Out = (),
>: Send + 'static
{
    type System: StartSystem<D, F, Out>;

    fn into_system(this: Self) -> Self::System;
}

pub struct Infallible;

impl<D, F, M, S, Out> IntoStartSystem<D, F, (Infallible, M), Out> for S
where
    S: IntoSystem<Start<'static, 'static, D, F>, Out, M> + Send + 'static,
    S::System: StartSystem<D, F, Out>,
    D: QueryData + QueryObserverAccess,
    F: QueryFilter + QueryObserverAccess,
{
    type System = S::System;

    fn into_system(this: Self) -> Self::System {
        IntoSystem::into_system(this)
    }
}

pub trait AddStartObserver {
    fn add_start_observer<S, D, F, M>(&mut self, system: S) -> &mut Self
    where
        S: IntoStartSystem<D, F, M, ()>,
        D: QueryData + QueryObserverAccess + 'static,
        F: QueryFilter + QueryObserverAccess + 'static;
}

impl AddStartObserver for bevy_app::App {
    fn add_start_observer<S, D, F, M>(&mut self, system: S) -> &mut Self
    where
        S: IntoStartSystem<D, F, M, ()>,
        D: QueryData + QueryObserverAccess + 'static,
        F: QueryFilter + QueryObserverAccess + 'static,
    {
        self.world_mut()
            .spawn_query_observer(QueryObserver::start(system));
        self
    }
}

impl super::QueryObserver {
    pub fn start<S, D, F, M>(observer: S) -> Self
    where
        S: IntoStartSystem<D, F, M, ()>,
        D: QueryData + QueryObserverAccess + 'static,
        F: QueryFilter + QueryObserverAccess + 'static,
    {
        let evaluator = |world: UnsafeWorldCell<'_>,
                         archetype: &Archetype,
                         kind,
                         triggered_by: &[_]| {
            D::evaluate_archetype(world, archetype, kind, triggered_by)
                && F::evaluate_archetype(world, archetype, kind, triggered_by)
                && DefaultFilterBanisher::evaluate_archetype(world, archetype, kind, triggered_by)
        };

        let kind = super::QueryObserverKind::Start;
        let access_getter = |world: &mut World, kind: super::QueryObserverKind| {
            let mut access = super::Access::default();
            D::report_access(world, kind, &mut access);
            F::report_access(world, kind, &mut access);
            DefaultFilterBanisher::report_access(world, kind, &mut access);
            access
        };

        let system = InfallibleStartSystem::new(IntoStartSystem::into_system(observer));

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

pub struct InfallibleStartSystem<S, D, F>
where
    D: QueryData + QueryObserverAccess + 'static,
    F: QueryFilter + QueryObserverAccess + 'static,
{
    input_state: Option<SystemState<Query<'static, 'static, D, DefaultFilterBanisher>>>,
    marker: PhantomData<fn() -> F>,
    system: S,
}

impl<D, F> InfallibleStartSystem<(), D, F>
where
    D: QueryData + QueryObserverAccess + 'static,
    F: QueryFilter + QueryObserverAccess + 'static,
{
    pub fn new<S>(system: S) -> InfallibleStartSystem<S, D, F>
    where
        S: System<In = Start<'static, 'static, D, F>>,
    {
        InfallibleStartSystem {
            input_state: None,
            marker: PhantomData,
            system: IntoStartSystem::into_system(system),
        }
    }
}

impl<S, D, F> InfallibleStartSystem<S, D, F>
where
    S: System<In = Start<'static, 'static, D, F>>,
    D: QueryData + QueryObserverAccess,
    F: QueryFilter + QueryObserverAccess,
{
    fn input_state(&self) -> &SystemState<Query<'static, 'static, D, DefaultFilterBanisher>> {
        self.input_state
            .as_ref()
            .expect("system must be initialized")
    }
}

impl<S, D, F> System for InfallibleStartSystem<S, D, F>
where
    S: System<In = Start<'static, 'static, D, F>>,
    D: QueryData + QueryObserverAccess,
    F: QueryFilter + QueryObserverAccess,
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
        let input_state = SystemState::<Query<D, DefaultFilterBanisher>>::new(world);

        let param_state = input_state.param_state();
        let component_access = param_state.component_access();

        let mut set = self.system.initialize(world);

        if !set.get_conflicts_single(component_access).is_empty() {
            panic!(
                "`DataAdded` access conflicts with one or more system parameters in `{}`",
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

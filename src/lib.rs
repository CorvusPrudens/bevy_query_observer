//! [![crates.io](https://img.shields.io/crates/v/bevy_query_observer)](https://crates.io/crates/bevy_query_observer)
//! [![docs.rs](https://docs.rs/bevy_query_observer/badge.svg)](https://docs.rs/bevy_query_observer)
//!
//! `bevy_query_observer` provides observers that triggers
//! when an entity starts or stops matching a query. Query
//! observers also trigger when a matching entity's data
//! changes according to lifecycle events.
//!
//! ```
//! # use bevy::prelude::*;
//! # use bevy_query_observer::*;
//! #[derive(Component)]
//! struct Red;
//!
//! #[derive(Component)]
//! struct Blue;
//!
//! fn red_and_blue(
//!     data: Start<Entity, (With<Red>, With<Blue>)>,
//!     mut commands: Commands,
//! ) {
//!     warn!("{:?} is both red and blue", *data);
//! }
//! ```
//!
//! The design is based on [Lifecycle event observers for queries](https://github.com/bevyengine/bevy/issues/20817).
//!
//! ## Getting started
//!
//! Since `bevy_query_observer` is implemented entirely in terms of lifecycle events,
//! all you need to get started is to add the dependency.
//!
//! ```toml
//! [dependencies]
//! bevy = "0.17.3"
//! bevy_query_observer = "0.2.0"
//! ```
//!
//! Query observers are added like normal observers.
//!
//! ```
//! # use bevy::prelude::*;
//! # use bevy_query_observer::*;
//! # #[derive(Component)]
//! # struct Red;
//! # #[derive(Component)]
//! # struct Blue;
//! # fn red_and_blue(data: Start<Entity, (With<Red>, With<Blue>)>) {}
//! fn plugin(app: &mut App) {
//!     app.add_start_observer(red_and_blue);
//! }
//! ```
//!
//! Like [`Observer`], you can spawn [`QueryObserver`] directly for greater control
//! over what it watches and where it's spawned.
//!
//! [`Observer`]: bevy_ecs::prelude::Observer
//!
//! ```
//! # use bevy::prelude::*;
//! # use bevy_query_observer::*;
//! # #[derive(Component)]
//! # struct Red;
//! # #[derive(Component)]
//! # struct Blue;
//! # fn red_and_blue(data: Start<Entity, (With<Red>, With<Blue>)>) {}
//! fn target_entity(mut commands: Commands) {
//!     let target = commands.spawn_empty().id();
//!
//!     let query_observer = QueryObserver::start(red_and_blue).with_entity(target);
//!     commands.spawn_query_observer(query_observer);
//! }
//! ```
//!
//! ## Maintaining non-trivial invariants
//!
//! Query observers make it easier to maintain non-trivial invariants.
//! This example, taken from
//! [the initial design discussion](https://github.com/bevyengine/bevy/issues/20817),
//! illustrates how you can maintain an entity index with excluding marker components.
//!
//! ```
//! # use bevy::{prelude::*, platform::collections::HashMap, ecs::entity::EntityHashSet};
//! # use bevy_query_observer::*;
//! #[derive(Component, Eq, PartialEq, Hash, Copy, Clone)]
//! #[component(immutable)]
//! struct Value(usize);
//!
//! #[derive(Component)]
//! struct ExcludeFromIndex;
//!
//! #[derive(Resource)]
//! struct Index(HashMap<Value, EntityHashSet>);
//!
//! fn add_to_index(
//!     event: Start<(Entity, &Value), Without<ExcludeFromIndex>>,
//!     mut index: ResMut<Index>,
//! ) {
//!     let (entity, value) = event.into_inner();
//!     index.0.entry(*value).or_default().insert(entity);
//! }
//!
//! fn remove_from_index(
//!     event: Stop<(Entity, &Value), Without<ExcludeFromIndex>>,
//!     mut index: ResMut<Index>,
//! ) {
//!     let (entity, value) = event.into_inner();
//!     index.0.entry(*value).or_default().remove(&entity);
//! }
//! ```
//!
//! [`Start`] and [`Stop`] handle default query filters automatically,
//! meaning the above implicitly accounts for [`Disabled`] and any
//! other default filter.
//!
//! Expressing the above in terms of normal observers requires at least four
//! hand-written observers. Each disabling component
//! requires two additional observers.
//!
//! ## Limitations
//!
//! Because `bevy_query_observer` is implemented in user space and `bevy_ecs` isn't
//! yet expressive enough, the crate comes with a few limitations.
//!
//! 1. Certain observers that require evaluation before an archetype change are faked.
//!
//!    For example, `Stop<(), Without<MyFilter>>` must run just before `MyFilter` is added
//!    to fulfill the semantics of [`Stop`]. However, Bevy's lifecycle events can't express
//!    this, so the query observer ignores `Without<MyFilter>` in this
//!    scenario when fetching the data. In practice this is usually fine,
//!    but it may deny simultaneous mutable access or break subtle invariants.
//!
//! 2. Only a subset of Bevy's built-in [`QueryData`] and [`QueryFilter`] types are supported.
//!
//!    While `bevy_query_observer` could spawn observers in terms of a [`Query`]'s access,
//!    Bevy's access types are private. As a result, [`Start`] and [`Stop`] depend
//!    on [`QueryObserverAccess`], meaning any custom query data or filters require
//!    an implementation.
//!
//! 3. Performance isn't great
//!
//!    The overhead of evaluating a query observer is around three times slower than
//!    a normal observer. Spawning query observers is nearly an order of magnitude slower.
//!
//! 4. Query observers frequently early return
//!
//!    Since each unique component in [`Start`] and [`Stop`] needs a dedicated observer,
//!    lifecycle events will frequently trigger query observer evaluations that do nothing.
//!    Default filters are included, so inserting or removing a disabling component
//!    may trigger many short-circuiting observers.
//!
//!    In principle, first party query observers could reduce unnecessary evaluations
//!    using archetype information.
//!
//! [`Disabled`]: bevy_ecs::prelude::Disabled
//! [`QueryObserverAccess`]: observer::QueryObserverAccess
//! [`QueryData`]: bevy_ecs::prelude::QueryData
//! [`QueryFilter`]: bevy_ecs::prelude::QueryFilter
//! [`Query`]: bevy_ecs::prelude::Query

#![no_std]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]

extern crate alloc;

pub mod observer;

pub use observer::{
    InsertQueryObserver, QueryObserver, SpawnQueryObserver,
    start::{AddStartObserver, Start},
    stop::{AddStopObserver, Stop},
};

#[cfg(test)]
mod test {
    // use super::*;
    use crate::observer::{
        start::{AddStartObserver, Start},
        stop::{AddStopObserver, Stop},
    };
    use bevy_app::prelude::*;
    use bevy_ecs::{entity::EntityHashSet, entity_disabling::Disabled, prelude::*};
    use bevy_platform::collections::HashMap;

    #[derive(Component)]
    struct TestMarker;

    #[derive(Component, Eq, PartialEq, Hash, Copy, Clone)]
    #[component(immutable)]
    struct Value(usize);

    #[derive(Resource, Default)]
    struct TestResource(usize);

    #[test]
    fn test_lifecycle_add() {
        let mut app = App::new();
        app.init_resource::<TestResource>();

        fn add(_: Start<(), (With<TestMarker>, With<Name>)>, mut res: ResMut<TestResource>) {
            res.0 += 1;
        }

        app.add_start_observer(add);

        let world = app.world_mut();
        let entity = world.spawn((TestMarker, Name::new("ww"))).id();
        // make sure it doesn't trip on a reinsert
        world.entity_mut(entity).insert(TestMarker);

        assert_eq!(world.resource::<TestResource>().0, 1);
    }

    #[test]
    fn test_lifecycle_insert() {
        let mut app = App::new();
        app.init_resource::<TestResource>();

        fn insert(_: Start<&TestMarker>, mut res: ResMut<TestResource>) {
            res.0 += 1;
        }

        app.add_start_observer(insert);

        let world = app.world_mut();
        let entity = world.spawn(TestMarker).id();
        world.entity_mut(entity).insert(TestMarker);

        assert_eq!(world.resource::<TestResource>().0, 2);
    }

    #[test]
    fn test_lifecycle_replace() {
        let mut app = App::new();
        app.init_resource::<TestResource>();

        fn replace(_: Stop<&TestMarker>, mut res: ResMut<TestResource>) {
            res.0 += 1;
        }

        app.add_stop_observer(replace);

        let world = app.world_mut();
        let entity = world.spawn(TestMarker).id();
        world.entity_mut(entity).insert(TestMarker);
        world.entity_mut(entity).remove::<TestMarker>();

        assert_eq!(world.resource::<TestResource>().0, 2);
    }

    #[test]
    fn test_lifecycle_remove() {
        let mut app = App::new();
        app.init_resource::<TestResource>();

        fn remove(_: Stop<(), With<TestMarker>>, mut res: ResMut<TestResource>) {
            res.0 += 1;
        }

        app.add_stop_observer(remove);

        let world = app.world_mut();
        let entity = world.spawn(TestMarker).id();
        world.entity_mut(entity).insert(TestMarker);
        world.entity_mut(entity).remove::<TestMarker>();

        assert_eq!(world.resource::<TestResource>().0, 1);
    }

    #[test]
    fn test_disabling() {
        let mut app = App::new();
        app.init_resource::<TestResource>();

        fn add(_: Start<(), With<TestMarker>>, mut res: ResMut<TestResource>) {
            res.0 += 1;
        }

        fn remove(_: Stop<(), With<TestMarker>>, mut res: ResMut<TestResource>) {
            res.0 -= 1;
        }

        app.add_start_observer(add);
        app.add_stop_observer(remove);

        let world = app.world_mut();
        let entity = world.spawn(TestMarker).id();
        world.entity_mut(entity).insert(Disabled);
        world.entity_mut(entity).remove::<TestMarker>();
        world.entity_mut(entity).insert(TestMarker);

        assert_eq!(world.resource::<TestResource>().0, 0);
    }

    #[test]
    fn test_invariants() {
        #[derive(Component)]
        struct ExcludeFromIndex;

        #[derive(Resource, Default)]
        struct Index(HashMap<Value, EntityHashSet>);

        fn add_to_index(
            event: Start<(Entity, &Value), Without<ExcludeFromIndex>>,
            mut index: ResMut<Index>,
        ) {
            let (entity, value) = event.into_inner();
            index.0.entry(*value).or_default().insert(entity);
        }

        fn remove_from_index(
            event: Stop<(Entity, &Value), Without<ExcludeFromIndex>>,
            mut index: ResMut<Index>,
        ) {
            let (entity, value) = event.into_inner();
            index.0.entry(*value).or_default().remove(&entity);
        }

        let mut app = App::new();

        app.add_start_observer(add_to_index);
        app.add_stop_observer(remove_from_index);
        app.init_resource::<Index>();

        let world = app.world_mut();

        let test_entity = world.spawn(Value(42)).id();

        // Test that the index is added.
        let index = world.resource::<Index>();
        assert!(index.0.get(&Value(42)).unwrap().contains(&test_entity));

        // Test that the index is removed when the exclude marker component is added.
        world.entity_mut(test_entity).insert(ExcludeFromIndex);
        let index = world.resource::<Index>();
        assert!(!index.0.get(&Value(42)).unwrap().contains(&test_entity));

        // Verify that the index is re-added when the exclude marker component is removed.
        world.entity_mut(test_entity).remove::<ExcludeFromIndex>();
        let index = world.resource::<Index>();
        assert!(index.0.get(&Value(42)).unwrap().contains(&test_entity));

        // Verify that the index at 42 is removed while 69 is added.
        world.entity_mut(test_entity).insert(Value(69));
        let index = world.resource::<Index>();
        assert!(!index.0.get(&Value(42)).unwrap().contains(&test_entity));
        assert!(index.0.get(&Value(69)).unwrap().contains(&test_entity));

        // Finally, verify that the index is removed when value is removed.
        world.entity_mut(test_entity).remove::<Value>();
        let index = world.resource::<Index>();
        assert!(!index.0.get(&Value(69)).unwrap().contains(&test_entity));
    }

    #[test]
    fn test_duplicate_filtering() {
        let mut app = App::new();
        app.init_resource::<TestResource>();

        fn add_and_insert(_: Start<&Value, With<TestMarker>>, mut res: ResMut<TestResource>) {
            res.0 += 1;
        }

        fn remove_and_replace(_: Stop<&Value, With<TestMarker>>, mut res: ResMut<TestResource>) {
            res.0 -= 1;
        }

        app.add_start_observer(add_and_insert);
        app.add_stop_observer(remove_and_replace);

        let world = app.world_mut();
        let test_entity = world.spawn((Value(42), TestMarker)).id();

        // without filtering, this would trigger twice
        assert_eq!(world.resource::<TestResource>().0, 1);

        world.despawn(test_entity);

        assert_eq!(world.resource::<TestResource>().0, 0);
    }
}

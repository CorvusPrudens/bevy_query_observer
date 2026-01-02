#![no_std]

extern crate alloc;

pub mod observer;

pub use observer::{
    InsertQueryObserver, QueryObserver, SpawnQueryObserver, TriggerQueryObserver,
    start::{AddStartObserver, Start},
    stop::{AddStopObserver, Stop},
};

#[derive(Clone, Copy)]
pub enum Source {
    Data,
    Filter(core::any::TypeId),
}

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

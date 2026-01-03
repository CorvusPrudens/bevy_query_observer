[![crates.io](https://img.shields.io/crates/v/bevy_query_observer)](https://crates.io/crates/bevy_query_observer)
[![docs.rs](https://docs.rs/bevy_query_observer/badge.svg)](https://docs.rs/bevy_query_observer)

`bevy_query_observer` provides observers that trigger
when an entity starts or stops matching a query. Query
observers also trigger when a matching entity's data
changes according to lifecycle events.

```rs
#[derive(Component)]
struct Red;

#[derive(Component)]
struct Blue;

fn red_and_blue(
    data: Start<Entity, (With<Red>, With<Blue>)>,
    mut commands: Commands,
) {
    warn!("{:?} is both red and blue", *data);
}
```

The design is based on [Lifecycle event observers for queries](https://github.com/bevyengine/bevy/issues/20817).

## Getting started

Since `bevy_query_observer` is implemented entirely in terms of lifecycle events,
all you need to get started is to add the dependency.

```toml
[dependencies]
bevy = "0.17.3"
bevy_query_observer = "0.2.0"
```

Query observers are added like normal observers.

```rs
fn plugin(app: &mut App) {
    app.add_start_observer(red_and_blue);
}
```

Like `Observer`, you can spawn `QueryObserver` directly for greater control
over what it watches and where it's spawned.

```rs
fn target_entity(mut commands: Commands) {
    let target = commands.spawn_empty().id();

    let query_observer = QueryObserver::start(red_and_blue).with_entity(target);
    commands.spawn_query_observer(query_observer);
}
```

## Maintaining non-trivial invariants

Query observers make it easier to maintain non-trivial invariants.
This example, taken from
[the initial design discussion](https://github.com/bevyengine/bevy/issues/20817),
illustrates how you can maintain an entity index with excluding marker components.

```rs
#[derive(Component, Eq, PartialEq, Hash, Copy, Clone)]
#[component(immutable)]
struct Value(usize);

#[derive(Component)]
struct ExcludeFromIndex;

#[derive(Resource)]
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
```

`Start` and `Stop` handle default query filters automatically,
meaning the above implicitly accounts for `Disabled` and any
other default filter.

Expressing the above in terms of normal observers requires at least four
hand-written observers. Each disabling component
requires two additional observers.

## Limitations

Because `bevy_query_observer` is implemented in user space and `bevy_ecs` isn't
yet expressive enough, the crate comes with a few limitations.

1. Certain observers that require evaluation before an archetype change are faked.

   For example, `Stop<(), Without<MyFilter>>` must run just before `MyFilter` is added
   to fulfill the semantics of `Stop`. However, Bevy's lifecycle events can't express
   this, so the query observer ignores `Without<MyFilter>` in this
   scenario when fetching the data. In practice this is usually fine,
   but it may deny simultaneous mutable access or break subtle invariants.

2. Only a subset of Bevy's built-in `QueryData` and `QueryFilter` types are supported.

   While `bevy_query_observer` could spawn observers in terms of a `Query`'s access,
   Bevy's access types are private. As a result, `Start` and `Stop` depend
   on `QueryObserverAccess`, meaning any custom query data or filters require
   an implementation.

3. Performance isn't great

   The overhead of evaluating a query observer is around three times greater than
   a normal observer. Spawning query observers is nearly an order of magnitude slower.

4. Query observers frequently early return

   Since each unique component in `Start` and `Stop` needs a dedicated observer,
   lifecycle events will frequently trigger query observer evaluations that do nothing.
   Default filters are included, so inserting or removing a disabling component
   may trigger many short-circuiting observers.

   In principle, first party query observers could reduce unnecessary evaluations
   using archetype information.

## Bevy version compatibility

| `bevy` | `bevy_query_observer` |
| ------ | --------------- |
| 0.17   | 0.1, 0.2        |

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>

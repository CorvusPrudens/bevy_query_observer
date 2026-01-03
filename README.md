# `bevy_query_observer`

`bevy_query_observer` provides query-like observers for Bevy.

```rs
#[derive(Component)]
struct Red;

#[derive(Component)]
struct Blue;

fn despawn_red_and_blue(
    data: Start<Entity, (With<Red>, With<Blue>)>,
    mut commands: Commands,
) {
    commands.entity(*data).despawn();
}
```

These observers allow you to establish and maintain
complex invariants across your entities.

The design of this crate is based on @chescock's
[Lifecycle event observers for queries](https://github.com/bevyengine/bevy/issues/20817)
proposal.

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

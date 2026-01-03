use bevy::prelude::*;
use bevy_query_observer::*;
use criterion::{Criterion, criterion_group, criterion_main};
use std::hint::black_box;

fn observer_benches(c: &mut Criterion) {
    fn simple_observer(trigger: On<Add, Name>) {
        black_box(trigger);
    }

    fn simple_query_observer(data: Start<Entity, With<Name>>) {
        black_box(data);
    }

    let mut group = c.benchmark_group("spawn");

    let iterations = 1000;
    group.bench_function("basic observer", |b| {
        let mut app = App::new();
        let mut world = app.world_mut();

        b.iter(|| {
            for _ in 0..iterations {
                let observer = Observer::new(simple_observer);
                world.spawn(observer);
                black_box(&mut world);
            }
        });
    });

    group.bench_function("query observer", |b| {
        let mut app = App::new();
        let mut world = app.world_mut();

        b.iter(|| {
            for _ in 0..iterations {
                let observer = QueryObserver::start(simple_query_observer);
                world.spawn_query_observer(observer);
                black_box(&mut world);
            }
        });
    });

    drop(group);

    let mut group = c.benchmark_group("evaluate");
    group.bench_function("no observers", |b| {
        let mut app = App::new();
        let mut world = app.world_mut();

        b.iter(|| {
            for _ in 0..iterations {
                world.spawn(Name::new("test"));
                black_box(&mut world);
            }
        });
    });

    group.bench_function("basic observer", |b| {
        let mut app = App::new();
        let mut world = app.world_mut();

        let observer = Observer::new(simple_observer);
        world.spawn(observer);

        b.iter(|| {
            for _ in 0..iterations {
                world.spawn(Name::new("test"));
                black_box(&mut world);
            }
        });
    });

    group.bench_function("query observer", |b| {
        let mut app = App::new();
        let mut world = app.world_mut();

        let observer = QueryObserver::start(simple_query_observer);
        world.spawn_query_observer(observer);

        b.iter(|| {
            for _ in 0..iterations {
                world.spawn(Name::new("test"));
                black_box(&mut world);
            }
        });
    });
}

criterion_group!(benches, observer_benches);
criterion_main!(benches);

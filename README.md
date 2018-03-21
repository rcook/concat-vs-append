# concat-vs-append by Richard Cook

Demonstrate benchmarking with [Criterion][criterion]

## Clone repository

```
git clone https://github.com/rcook/concat-vs-append.git
```

## Install compiler

```
stack setup
```

## Build, run and generate HTML report

```
stack clean
stack build
stack exec -- concat-vs-append --output bench.html
```

## Run tests

```
stack test
```

## Licence

Released under [MIT License][licence]

[criterion]: http://www.serpentine.com/criterion/tutorial.html
[licence]: LICENSE

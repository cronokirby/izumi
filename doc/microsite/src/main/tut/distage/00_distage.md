---
out: index.html
---

distage: Staged Dependency Injection
====================================

`distage` is a pragmatic module system for Scala that combines simplicity and reliability of pure FP with extreme late-binding
and flexibility of runtime dependency injection frameworks such as Guice.

`distage` is unopinionated, it's good for structuring applications written in either imperative Scala style or in pure FP @ref[Tagless Final Style](basics.md#tagless-final-style)

```scala mdoc:invisible
// or with [ZIO Environment](#zio).
```

Why use distage?
-------------------

1. **Faster applications and tests**: 
    `distage` guarantees that no unneccessary instantiations will happen during your tests or application startup.
2. **Quicker failure detection**: 
    `distage` peforms all the integration checks for your application and tests even before any instantiations happened.    
3. **Simpler tests**: 
    `distage` eliminates all the hard work of setting up your test environments, especially polymorphic ones.
4. **Simpler development workflows**: 
    `distage` allows you to develop Role-Based Applications -- a fusion of Microservices and Monoliths.
     You may run all your services in one process for development purposes (and even switch to mock implementations with a single commandline argument).
5. **Faster compile times and low mental overhead**:
    Unlike fully compile-time DIs, `distage` does not impose a compile time penalty for medium/large projects.
    `distage` extensions are simple to write and do not require type-level programming.
6. **Simpler deployments**:
    `distage` allows you to simplify deployments and reduce your running costs by leveraging Role-Based Applications Architecture.
7. **Simpler debugging**:
    `distage` provides you important insights about your application and allows you to introspect and modify it on the fly, 
    before any instantiations happen.
8. **Higher Correctness**: 
    `distage` supports resources and lifecycle natively and guarantees proper cleanups even when something went wrong.
9. **Cheaper integration tests**:
    `distage` provides you great memoization support for your tests so you may reuse expensive resources (like database connections) across multiple
    integration tests, gaining performance and without sacrificing correctness.
10. **No pressure**: 
    `distage` is non-invasive and designed to not impact the way your Scala code is written. 
    It just removes all the initialization boilerplate.
    You don't need to learn magic tricks to write components in a distage application.

`distage` is recommended by industry leaders:

> Given its native support for type classes and higher-kinded types -- both features indispensable to functional programming -- distage is one of the leading dependency injection libraries out there. Bonus points for being built by a wicked-smart team that contributes to ZIO! 
> 
> -– *John A. De Goes*

FAQ
---

**Q**: how to pronounce "distage" ?

**A**: 'Dee-stage'! Although, you may pronounce it however you like.

**Q**: isn't it unsafe to use a runtime dependency injection framework?

**A**: `distage` has static verification mechanisms allowing to achieve same level of guarantees as hand-written static code.
        Full compile-time DI support is under development.
 
Further reading
---------------

- [distage Example Project](https://github.com/7mind/distage-sample)
- Slides - [distage: Staged Dependency Injection](https://www.slideshare.net/7mind/scalaua-distage-staged-dependency-injection)
- @ref[Tutorial](basics.md#tutorial)
- @ref[Config Injection](config_injection.md)
- @ref[Other features](other-features.md)
- @ref[Debugging](debugging.md)
- @ref[Cookbook](cookbook.md)
- @ref[Role-Based Apps](roles.md)
- @ref[Testkit](testkit.md)
- @ref[Syntax reference](reference.md)

## PPER

See @ref[PPER Overview](../pper/00_pper.md)

@@@ index

* [Basics](basics.md)
* [Config Injection](config_injection.md)
* [Other features](other-features.md)
* [Debugging](debugging.md)
* [Cookbook](cookbook.md)
* [Role-Based Apps](roles.md)
* [Testkit](testkit.md)
* [Syntax reference](reference.md)

@@@

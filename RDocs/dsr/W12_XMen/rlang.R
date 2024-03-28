# Let's create some symbols:
foo <- quote(foo)
bar <- sym("bar")

# as_name() converts symbols to strings:
foo
rlang::as_name(foo)
typeof(foo)

typeof(bar)
typeof(rlang::as_name(bar))

# as_name() unwraps quosured symbols automatically:
rlang::as_name(quo(foo))

cname <- quote("cname")
rlang::as_name(cname)
quote(cname)

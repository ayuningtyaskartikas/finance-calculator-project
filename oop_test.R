# ============================================================
# oop_test.R — Inheritance & Dispatch Test Programs
# Tests: Variable Inheritance (Example 3)
#        Method Dispatch     (Example 5)
# ============================================================

library(methods)


# ── EXAMPLE 3: Variable Inheritance ──────────────────────────

cat("========================================\n")
cat("  EXAMPLE 3: Variable Inheritance\n")
cat("========================================\n")

c1 <- setRefClass("c1",
  fields = list(x = "numeric", y = "numeric"),
  methods = list(
    initialize = function() { x <<- 0; y <<- 0 },
    setx1 = function(v) { x <<- v },
    sety1 = function(v) { y <<- v },
    getx1 = function()  { x },
    gety1 = function()  { y }
  )
)

c2 <- setRefClass("c2",
  contains = "c1",
  fields = list(y = "numeric"),
  methods = list(
    sety2 = function(v) { y <<- v },
    getx2 = function()  { x },
    gety2 = function()  { y }
  )
)

o2 <- c2$new()
o2$setx1(101)
o2$sety1(102)
o2$sety2(999)

cat("getx1():", o2$getx1(), "\n")
cat("gety1():", o2$gety1(), "\n")
cat("getx2():", o2$getx2(), "\n")
cat("gety2():", o2$gety2(), "\n")
cat("\n→ If gety1() = 999: DYNAMIC variable inheritance\n")
cat("→ If gety1() = 102: STATIC variable inheritance\n\n")


# ── EXAMPLE 5a: Method Dispatch (self + super) ───────────────

# helper so d2 can call d1's m1 logic directly (equivalent to "super m1()")
d1_m1 <- function(self_obj) { self_obj$m2() }

d1 <- setRefClass("d1",
  methods = list(
    initialize = function() { 1 },
    m1 = function() { d1_m1(.self) },
    m2 = function() { 13 }
  )
)

d2 <- setRefClass("d2",
  contains = "d1",
  methods = list(
    m1 = function() { 22 },
    m2 = function() { 23 },
    m3 = function() { d1_m1(.self) }
  )
)

d3 <- setRefClass("d3",
  contains = "d2",
  methods = list(
    m1 = function() { 32 },
    m2 = function() { 33 }
  )
)

o3 <- d3$new()
cat("send o3 m3():", o3$m3(), "\n")
cat("\n→ If result = 33: DYNAMIC method dispatch\n")
cat("→ If result = 13: STATIC method dispatch\n\n")

# ── EXAMPLE 5b: Method Dispatch (self + fields) ──────────────

cat("========================================\n")
cat("  EXAMPLE 5b: Method Dispatch + Fields\n")
cat("========================================\n")

e1 <- setRefClass("e1",
  fields = list(x = "numeric", y = "numeric"),
  methods = list(
    initialize = function() { x <<- 1; y <<- 2 },
    get = function() { x },
    m1  = function() { .self$get() }
  )
)

e2 <- setRefClass("e2",
  contains = "e1",
  fields = list(x = "numeric", y = "numeric"),
  methods = list(
    initialize = function() { callSuper(); x <<- 3; y <<- 4 },
    get = function() { y }
  )
)

o2b <- e2$new()
cat("send o2 m1():", o2b$m1(), "\n")
cat("\n→ If result = 4: DYNAMIC method dispatch (self uses runtime type)\n")
cat("→ If result = 1: STATIC method dispatch\n")
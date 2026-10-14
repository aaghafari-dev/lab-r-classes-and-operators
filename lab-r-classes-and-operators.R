
#================================== # Lab | Classes, functions & operators in R

##================================== Task 1: Create a Custom S3 Class

#==== 1. Define a custom S3 class called "Circle" that represents a circle. The class should store the radius of the circle.

# ==================================
# Lab | Classes, functions & operators in R
# ==================================

## ==================================
## Task 1: Create a Custom S3 Class
## ==================================

# ---- 1 & 2. Define a custom S3 class "Circle" and a constructor function ----
# The constructor stores the radius and (optionally) the center coordinates.
Circle <- function(radius, x = 0, y = 0) {
  structure(
    list(radius = radius, x = x, y = y),
    class = "Circle"
  )
}

# ---- 3. print.Circle() method ----
# Displays the radius and area of the circle when the object is printed.
print.Circle <- function(x, ...) {
  area <- pi * x$radius^2
  cat('Type: "Circle object"\n')
  cat("Radius:", x$radius, "\n")
  cat("Center: (", x$x, ",", x$y, ")\n", sep = "")
  cat("Area:", area, "\n")
}

# ---- Quick test for Task 1 ----
my_circle <- Circle(5)
print(my_circle)
# Expected output:
# Type: "Circle object"
# Radius: 5
# Center: (0,0)
# Area: 78.53982


## ==================================
## Task 2: Define a Custom Operator
## ==================================

# ---- 1. Custom operator %>% (distance + intersection check) ----
# NOTE: The README shows `%>%`, but that symbol is already used by magrittr.
# In R, custom infix operators are written as %<name>%, so we define `%>%`
# exactly as the lab asks. If magrittr is loaded, it will mask this, so
# run this file without loading magrittr (or use `%>%` carefully).
`%>%` <- function(c1, c2) {
  # Distance between centers
  distance <- sqrt((c2$x - c1$x)^2 + (c2$y - c1$y)^2)
  # Two circles intersect if distance <= sum of radii
  return(distance <= (c1$radius + c2$radius))
}

# ---- 2. Circle already includes x and y (see Task 1) ----

# ---- Quick test for Task 2 ----
circle1 <- Circle(radius = 3, x = 0, y = 0)
circle2 <- Circle(radius = 4, x = 5, y = 0)
circle1 %>% circle2
# Expected output: TRUE


## ==================================
## Task 3: Write a Function for the Class
## ==================================

# ---- 1. circumference() function ----
# Formula: circumference = 2 * π * radius
circumference <- function(circle) {
  UseMethod("circumference")
}

# ---- 2. Add it as a method for the Circle class ----
circumference.Circle <- function(circle) {
  return(2 * pi * circle$radius)
}

# ---- (Optional) default method for safety ----
circumference.default <- function(circle) {
  stop("circumference() only works on Circle objects.")
}

# ---- Quick test for Task 3 ----
my_circle <- Circle(radius = 5)
circumference(my_circle)
# Expected output: 31.41593


## ==================================
## Task 4: Create a Custom S4 Class
## ==================================

# ---- 1. Define the S4 class "Rectangle" ----
setClass(
  "Rectangle",
  slots = list(length = "numeric", width = "numeric")
)

# ---- 2. Constructor function Rectangle() ----
Rectangle <- function(length, width) {
  new("Rectangle", length = length, width = width)
}

# ---- 3. show() method for the Rectangle class ----
setMethod(
  "show", "Rectangle",
  function(object) {
    area <- object@length * object@width
    cat('Type: "Rectangle object"\n')
    cat("Length:", object@length, "\n")
    cat("Width:", object@width, "\n")
    cat("Area:", area, "\n")
  }
)

# ---- Quick test for Task 4 ----
my_rectangle <- Rectangle(length = 4, width = 6)
show(my_rectangle)
# Expected output:
# Type: "Rectangle object"
# Length: 4
# Width: 6
# Area: 24


## ==================================
## Task 5: Define a Custom Operator for the S4 Class
## ==================================

# ---- 1. Custom operator %==% compares the areas of two Rectangles ----
`%==%` <- function(r1, r2) {
  # Guard: both objects must be Rectangles
  if (!is(r1, "Rectangle") || !is(r2, "Rectangle")) {
    stop("%==% requires two Rectangle objects.")
  }
  area1 <- r1@length * r1@width
  area2 <- r2@length * r2@width
  return(area1 == area2)
}

# ---- Quick test for Task 5 ----
rectangle1 <- Rectangle(length = 4, width = 6)
rectangle2 <- Rectangle(length = 3, width = 8)
rectangle1 %==% rectangle2
# Expected output: TRUE


## ==================================
## Extra sanity checks (optional)
## ==================================

# Non-intersecting circles
c3 <- Circle(radius = 1, x = 0, y = 0)
c4 <- Circle(radius = 1, x = 10, y = 0)
c3 %>% c4          # FALSE

# Touching circles (distance == sum of radii → TRUE)
c5 <- Circle(radius = 2, x = 0, y = 0)
c6 <- Circle(radius = 3, x = 5, y = 0)
c5 %>% c6          # TRUE

# Rectangles with different areas
r3 <- Rectangle(length = 2, width = 2)
r4 <- Rectangle(length = 3, width = 3)
r3 %==% r4         # FALSE
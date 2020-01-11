Riddler Gematria
================
Alex Rossell Hayes
January 10, 2020

# The problem

This Riddler sets up an English version of Gematria, in which letters
are scored based on their place in the alphabet. A is 1, B is 2, etc.
Our task is to find the largest number which, when expressed in words,
has a gematria score higher than its numerical value. ‘One’, with a
score of 34, fits this criteria, while ‘one thousand’, with a score of
136, does not.

# Creating a function

In order to solve this, we need to create a function that finds the
English Gematria score for a given number.

The first step in achieving this uses the package `english`, which
allows us to convert an integer to its representation in English words.

``` r
library(english)

as.english(52)
```

    ## [1] fifty-two

Because the Gematria system doesn’t give any value to punctuation
characters, let’s get rid of them using the `stringr` package. The
regular expression “\\W” finds all ‘non-word’ characters, meaning
anything that is not a letter, digit, or underscore.

``` r
library(stringr)

as.english(52) %>%
  str_remove("\\W")
```

    ## [1] "fiftytwo"

Because scores are calculated at the individual character level, let’s
split the word from a single string to a vector of characters.
`str_split()` is designed to split one string into multiple strings at a
specified break point. For example, a month-day-year could be split at
the dashes:

``` r
str_split("01-10-2020", "-")
```

    ## [[1]]
    ## [1] "01"   "10"   "2020"

However, by using an empty string ("") as our split pattern, we can
divide the string into individual characters. This puts the string into
a list, so let’s also call `unlist()` to make it a single vector.

``` r
as.english(52) %>%
  str_remove("\\W") %>%
  str_split("") %>%
  unlist()
```

    ## [1] "f" "i" "f" "t" "y" "t" "w" "o"

We need to find each letter’s place in the alphabet. To do that, we can
compare the letters to R’s built-in `letters` variable, which contains
each letter in alphabetical order.

``` r
letters
```

    ##  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
    ## [20] "t" "u" "v" "w" "x" "y" "z"

To find a letter’s place, we can use the `which()` function to find
which character in letters equals our desired character.

``` r
which(letters == "g")
```

    ## [1] 7

To find the values for each character in our vector, we can use the
`furrr` package, which allows us to apply a function to each element in
a set. `furrr` works just like the better-known `purrr` package, but
uses parallel computing processes to speed up our analysis

``` r
library(furrr)
```

    ## Loading required package: future

``` r
plan(multiprocess) # Tell furrr to use multiple processes to speed up analysis

as.english(52) %>%
  str_remove("\\W") %>%
  str_split("") %>%
  unlist() %>%
  future_map_int(~ which(letters == .))
```

    ## [1]  6  9  6 20 25 20 23 15

Finally, we use `sum()` to add up the values, giving us the Gematria
score for the whole word.

``` r
as.english(52) %>%
  str_remove("\\W") %>%
  str_split("") %>%
  unlist() %>%
  future_map_int(~ which(letters == .)) %>%
  sum()
```

    ## [1] 124

Putting this all together, we can make a function that calculates the
Gematria score for an arbitrary number:

``` r
gematria <- function(integer) {
  integer %>%
    as.english() %>%
    str_remove_all("\\W") %>%
    str_split("") %>%
    unlist() %>%
    future_map_int(~ which(letters == .)) %>%
    sum()
}

gematria(538)
```

    ## [1] 265

# Narrowing the range

Now that we have a function, we can apply it to a list of numbers to get
our answer. But the question asks for the largest number that fits our
criteria. With infinite numbers, we can’t exhaustively test them all.
Let’s try to set an upper limit for our analysis.

The most points that can be scored by a single digit is the 65 points
scored by 7:

``` r
future_map_int(1:9, gematria) %>% `names<-`(1:9)
```

    ##  1  2  3  4  5  6  7  8  9 
    ## 34 58 56 60 42 52 65 49 42

Similarly, 70 does best in the tens place:

``` r
future_map_int(1:9 * 10, gematria) %>% `names<-`(1:9 * 10)
```

    ##  10  20  30  40  50  60  70  80  90 
    ##  39 107 100  84  66  97 110  74  87

It seems then that the maximum score for each number of digits will be
scored by a string of successive 7s. Let’s see how they score for one to
four digits:

``` r
future_map_int(c(7, 77, 777, 7777), gematria) %>% `names<-`(c(7, 77, 777, 7777))
```

    ##    7   77  777 7777 
    ##   65  175  314  481

7 and 77 have higher scores than their values, while 777 and 7777’s
scores are too low.

This shows us that all three and four digit numbers do not have scores
that are more than three digits. It also shows us that this function has
diminishing returns: each 10-fold increase only increases the score by
less than 200.

Given this information, we can be confident that the answer must be
somewhere between 77 and 777. This gives us a reasonable range in which
to test our function.

# The analysis

Using `dplyr`, we can make a tibble with a row for each number between
777 and 77 (descending because we’re interested in the highest number).

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
(df <- tibble(number = 777:77))
```

    ## # A tibble: 701 x 1
    ##    number
    ##     <int>
    ##  1    777
    ##  2    776
    ##  3    775
    ##  4    774
    ##  5    773
    ##  6    772
    ##  7    771
    ##  8    770
    ##  9    769
    ## 10    768
    ## # … with 691 more rows

For each number, we `mutate()` to add a column with its Gematria score.

``` r
(df <- df %>% mutate(gematria_score = future_map_int(number, gematria)))
```

    ## # A tibble: 701 x 2
    ##    number gematria_score
    ##     <int>          <int>
    ##  1    777            314
    ##  2    776            301
    ##  3    775            291
    ##  4    774            309
    ##  5    773            305
    ##  6    772            307
    ##  7    771            283
    ##  8    770            249
    ##  9    769            278
    ## 10    768            285
    ## # … with 691 more rows

Finally, we `filter()` our results to only include those with a Gematria
score greater than the numerical value.

``` r
(df <- df %>% filter(gematria_score > number))
```

    ## # A tibble: 177 x 2
    ##    number gematria_score
    ##     <int>          <int>
    ##  1    279            284
    ##  2    278            291
    ##  3    277            307
    ##  4    276            294
    ##  5    275            284
    ##  6    274            302
    ##  7    273            298
    ##  8    272            300
    ##  9    271            276
    ## 10    269            271
    ## # … with 167 more rows

We see that the highest number left in the tibble is 279, so this is our
answer\!

# The answer

**279** is the largest number with a Gematria score larger than its
numerical value.

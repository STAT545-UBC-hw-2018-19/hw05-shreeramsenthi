---
title: "Factor and figure management"
author: Shreeram Senthivasan
output: github_document
---



# Intro

For this assignment, we will be exploring how to manipulate factors, improve figures and work with file input and output. To this end, I will be using the datasets compiled and provided by Joey Bernhardt in the [`singer`](https://github.com/JoeyBernhardt/singer) package.

# Part 1: Factor management

## Setup

Before we begin, let's make a more manageable subset of the `singer` dataset, rename some variables, and convert relevant variables to factors.


```r
song <- singer_locations %>%
  select(title,
         album = release,
         artist = artist_name,
         year,
         popularity = artist_hotttnesss,
         lat = latitude,
         long = longitude) %>%
  mutate(album = as_factor(album),
         artist = as_factor(artist),
         year = factor(year))

song %>%
  head %>%
  kable(col.names = c("Song Title", "Album", "Artist", "Year", "Popularity", "Latitude", "Longitude"))
```



|Song Title            |Album               |Artist                         |Year | Popularity| Latitude| Longitude|
|:---------------------|:-------------------|:------------------------------|:----|----------:|--------:|---------:|
|The Conversation (Cd) |Even If It Kills Me |Motion City Soundtrack         |2007 |  0.6410183|       NA|        NA|
|Lonely Island         |The Duke Of Earl    |Gene Chandler                  |2004 |  0.3937627| 41.88415| -87.63241|
|Here's That Rainy Day |Imprompture         |Paul Horn                      |1998 |  0.4306226| 40.71455| -74.00712|
|Rego Park Blues       |Still River         |Ronnie Earl & the Broadcasters |1995 |  0.3622792|       NA|        NA|
|Games                 |Afro-Harping        |Dorothy Ashby                  |1968 |  0.4107520| 42.33168| -83.04792|
|More Pipes            |Six Yanks           |Barleyjuice                    |2006 |  0.3762635| 40.99471| -77.60454|

Notice that the song titles shouldn't be coerced to a factor since they are all unique, unlike the other three variables we converted. Similarly `popularity`, `latitude`, and `longitude` should not be treated as factors since they are continuous, unlike `year`.

## Quality control

Let's check the levels of `year` to see the scope of the dataset:


```r
levels(song$year)
```

```
##  [1] "0"    "1922" "1926" "1927" "1929" "1937" "1940" "1945" "1947" "1948"
## [11] "1951" "1952" "1953" "1954" "1955" "1956" "1957" "1958" "1959" "1960"
## [21] "1961" "1962" "1963" "1964" "1965" "1966" "1967" "1968" "1969" "1970"
## [31] "1971" "1972" "1973" "1974" "1975" "1976" "1977" "1978" "1979" "1980"
## [41] "1981" "1982" "1983" "1984" "1985" "1986" "1987" "1988" "1989" "1990"
## [51] "1991" "1992" "1993" "1994" "1995" "1996" "1997" "1998" "1999" "2000"
## [61] "2001" "2002" "2003" "2004" "2005" "2006" "2007" "2008" "2009" "2010"
```

Looks like there was a typo and some entries have a release year of 0. Let's filter out these results:


```r
song  <- filter(song, year != "0") %>%
  mutate(year = fct_drop(year)) # Don't forget to drop unused levels if you don't want them around
levels(song$year) # Sure enough, the unwanted levels are gone
```

```
##  [1] "1922" "1926" "1927" "1929" "1937" "1940" "1945" "1947" "1948" "1951"
## [11] "1952" "1953" "1954" "1955" "1956" "1957" "1958" "1959" "1960" "1961"
## [21] "1962" "1963" "1964" "1965" "1966" "1967" "1968" "1969" "1970" "1971"
## [31] "1972" "1973" "1974" "1975" "1976" "1977" "1978" "1979" "1980" "1981"
## [41] "1982" "1983" "1984" "1985" "1986" "1987" "1988" "1989" "1990" "1991"
## [51] "1992" "1993" "1994" "1995" "1996" "1997" "1998" "1999" "2000" "2001"
## [61] "2002" "2003" "2004" "2005" "2006" "2007" "2008" "2009" "2010"
```

## Reordering factors

Perfect, and conveniently the levels of `year` are sorted alphabetically, which is probably what we would want if we were plotting something against `year`. Our other factors (`artist` and `album`) however do not have such a natural ordering. Instead, we can sort them based on another variable, such as the popularity of the artist.


```r
song <- song %>%
  mutate(artist = fct_reorder(artist, -popularity), # Negatives are a convenient way of reversing the sort order
         album = fct_reorder(album, -popularity)) # Here I use it since I want more popular artists first
```

Notice that rearranging the rows of the dataframe won't help since it doesn't change the underlying integers that the factors code for. But did this reordering do what we want? Let's use a plot to check.


```r
#Since we will be making a similar plot a couple times, let's make a function out of it!
pop_plot <- function(df){
  ggplot(df, aes(popularity, artist)) +
    geom_point(alpha = 0.2, size = 0.8) +
    theme_classic() +
    labs(x = "Popularity", y = "Artist, Reordered by Popularity") +
    theme(axis.text.y = element_blank(), #Remove the long list of artist names
          axis.ticks.y = element_blank() #and their corresponding ticks
          )
}

pop_plot(song)
```

![plot of chunk reorderedPlots](figure/reorderedPlots-1.png)

Sure enough, it appears that `artist` (and presumably `album`) have been reordered in a more meaningful way.

# Part 2: File I/O

Let's try saving and retrieving our subsetted dataset with the reordered factors. A standard option would be to write to csv.


```r
write_csv(song, "song.csv")
song2 <- read_csv("song.csv")
```

```
## Parsed with column specification:
## cols(
##   title = col_character(),
##   album = col_character(),
##   artist = col_character(),
##   year = col_integer(),
##   popularity = col_double(),
##   lat = col_double(),
##   long = col_double()
## )
```

```r
identical(song, song2) # Are they the same?
```

```
## [1] FALSE
```

```r
pop_plot(song2)
```

![plot of chunk ioCSV](figure/ioCSV-1.png)

As we can see, saving the dataset to csv does not allow us to retrieve factor information. This makes sense since the csv format does not have an obvious way of encoding both a value and a level to a single observation.

Instead, if we need to preserve factor information, we can use the R data structure format or RDS.


```r
saveRDS(song, "song.RDS")
song2 = readRDS("song.RDS")

identical(song, song2) # Are they the same?
```

```
## [1] TRUE
```

```r
pop_plot(song2)
```

![plot of chunk ioRDS](figure/ioRDS-1.png)

# Part 3: Visualization Design

While I suppose I could remake a plot I have already made, [MielleM](https://github.com/MielleM) mentioned in [her review](https://github.com/STAT545-UBC-students/hw04-shreeramsenthi/issues/1) of my last homework submission that mapping is a reasonable option in `ggplot2`. Well that's news to me! So why not try it out here since the `singer` dataset provides co-ordinates for different artists. More specifically, let's visualize where most artists are from.

## Double Check Assumptions

Before we begin, let's double check that there's only ever one pair of coordinates for each artist:


```r
song %>%
  filter(!is.na(lat)) %>%                                 # Let's start by removing all rows without coordinates
  mutate(lat = factor(lat), long = factor(long)) %>%      #One way to test for uniqueness is counting factor levels
  group_by(artist) %>%
  summarize(nlat = lat %>% droplevels %>% nlevels,        # You can use pipes in argument definitions too!
            nlong = long %>% droplevels %>% nlevels) %T>% # This is the T-pipe, it let's you output things in the middle of a pipe chain
  {
    writeLines("\nLet's see how many unique latitudes and longitudes are associated with each artist: ")
    print(.)
  } %>%
  filter(nlat != 1 | nlong != 1) %>%
  {
    writeLines("\nLet's use filter to see which artists have more than one unique pair of coordinates:")
    print(.)
  }
```

```
## 
## Let's see how many unique latitudes and longitudes are associated with each artist: 
## # A tibble: 2,956 x 3
##    artist             nlat nlong
##    <fct>             <int> <int>
##  1 Rihanna               1     1
##  2 Rihanna / Slash       1     1
##  3 Michael Jackson       1     1
##  4 T.I.                  1     1
##  5 Taylor Swift          1     1
##  6 Jason Aldean          1     1
##  7 Bruce Springsteen     1     1
##  8 The Killers           1     1
##  9 Linkin Park           1     1
## 10 Kings Of Leon         1     1
## # ... with 2,946 more rows
## 
## Let's use filter to see which artists have more than one unique pair of coordinates:
## # A tibble: 0 x 3
## # ... with 3 variables: artist <fct>, nlat <int>, nlong <int>
```

And sure enough, none of the artists have more than one unique pair of coordinates. Accordingly, we can summarize the dataset by artist and plot where most artists are from.

## Mapping

Let's get right to it!


```r
artist_by_location <- song %>%
  group_by(artist) %>%
  summarize(lat = first(lat), long = first(long)) %>% # This is only valid since we know that coordinates are unique per artist
  filter(!is.na(lat))

p <- artist_by_location %>%
  ggplot(aes(long, lat)) +
    geom_polygon(data = map_data("world"), aes(group = group), fill = "grey70") + # First we plot the map data from the package `mapdata`
    geom_point(size = 0.2, alpha = 0.2, colour = "red") + # Then we overlay the points
    theme_void() + # Let's get rid of the axes
    coord_fixed() # And fix the scale for the coordinates

ggplotly(p)
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

Well that's certainly a start, but it's hard to see trends when we try to look at the world as a whole. Not to mention, many areas are completely overplotted. Let's zoom in on the British Isles and try again.


```r
#Let's store the map data into an easier variable to call multiple times
brit_isle <- map_data('worldHires', c('UK', 'Ireland', 'Isle of Man','Isle of Wight', 'Wales:Anglesey')) %>%
  filter(lat > 50, lat < 60, long > -20, long < 3)

brit_isle_artists <- artist_by_location %>%
  filter(lat > 50, lat < 60, long > -20, long < 2) # Filter out artists from other countries

brit_isle_artists %>%
  ggplot(aes(long, lat)) + #As before
    geom_polygon(data = brit_isle, aes(group = group), fill = "grey70") +
    geom_point(alpha = 0.2, colour = "red") +
    theme_void() +
    coord_fixed(1.3)
```

![plot of chunk mapUK](figure/mapUK-1.png)

While this is much clearer, we still have not completely overcome the issue of overplotting. Although a little clunky, let's try using hexplots overlaid onto a map.


```r
hex_map <- brit_isle_artists %>%
  ggplot(aes(long, lat)) + #As before
    geom_polygon(data = brit_isle, aes(group = group), fill = "grey70") +
    stat_binhex(aes(fill=log(..count..)), bins = 50, alpha = 0.8, colour = "black") +
    scale_fill_gradient(low = "grey70", high = "red") +
    theme_void() +
    labs(fill = "Log Count\nof Artists") +
    coord_fixed(1.3)

hex_map
```

![plot of chunk mapHex](figure/mapHex-1.png)

I'm actually pretty happy with that. Let's stop here.

# Part 4: Writing figures to file

Now finally let's save the plot to a file using `ggsave`.


```r
ggsave("figure/hex_map.png", hex_map)
```

```
## Saving 7 x 7 in image
```

And now we can use standard `markdown` to show the image here:

![Figure 1](https://raw.githubusercontent.com/STAT545-UBC-students/hw05-shreeramsenthi/master/figure/hex_map.png)

But if you are publishing to the web (as we are here), and do not know the resolution the audience will view your document in, in can be useful to export graphics to vector formats when possible. This will allow readers to zoom in indefinitely without encountering the dreaded blocky graphics. Let's try it here:


```r
ggsave("figure/hex_map.svg", hex_map) # requires package `svglite` to save as svg
```

```
## Saving 7 x 7 in image
```

![Figure 2: A vectorized heat map of artists from the British Isles](https://raw.githubusercontent.com/STAT545-UBC-students/hw05-shreeramsenthi/master/figure/hex_map.svg?sanitize=true)

Oddly, it seems like the rendering of the hexagons and the fill legend didn't work. I've messed around with it a while but I can't seem to figure out why. Well, at least the plot *is* a vector image.


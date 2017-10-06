## Installation

```{r}
library(devtools)
install_github("crowding/msgpack-r")
```

## Usage

```{r}
library(msgpack)
packMsg( list(compact=TRUE, schema=0) )
unpackMsg( .Last.value )
```

## Connections

```{r}
"words"
```

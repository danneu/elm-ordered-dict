
# Insertion-Ordered Dictionaries for Elm

`OrderedDict` has the same API as Elm's `Dict` but with an added
function `compact` that removes potential holes in the underlying
datastructure.

More info is available in `OrderedDict.elm`.

This is a naive implementation that I threw together as a quick solution for
a project I'm working on.

``` elm
import OrderedDict exposing (OrderedDict)

users : OrderedDict String User
users :
  -- Insertion order is preserved
  OrderedDict.fromList
    [ ("foo", User 7 "foo")
    , ("bar", User 2 "bar")
    , ("qux", User 4 "qux")
    ]
  -- Updating a key that exists does not change insertion order
  |> OrderedDict.insert "bar" (User 2 "bob") 
  |> OrderedDict.remove "not-there"
  -- Removing a key and re-inserting it puts it at the end
  |> OrderedDict.remove "foo"
  |> OrderedDict.insert "foo" (User 7 "foo")
  |> OrderedDict.toList

--> [("bar", User 2 "bob"), ("qux", User 4 "qux"), ("foo", User 7 "foo")]
```

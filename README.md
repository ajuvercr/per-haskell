# per-haskell

## Workings

### Data types

bla bla

### Move calculation

Dynamically determine how many actions to make (timeout just before end).

One action can be 2 things:

- Send ships from multiple planets to capture a non-friendly planet
- Send shipt from multiple planets to defend a friendly planet that is attacked

Calculate all actions and apply the best action to the state.
Repeat until are no actions or timeout is exceeded.


### Possible addons

Use 'ranges', take for example 2 friendly planets can work together to capture a non-friendly planet. Ships from both planets together have to exceed the current amount of ships. But in what ratio is still in the air. But they are expressible in 'ranges' and 'constraints'.

So, applying an action triggers a list of constraints transforming the constraints state. These constraints transformation trigger again a list of other constraints etc, until everything is applied or something failed. Constrains state monad + Maybe () return type.

So best action maybe be unapplicable, so apply first action that is applicable.

Note: all actions in the first round are applicable.


### Ordering actions

Multiple options in handling this.

- `Action -> (Float, Float, ...)` and just sort on this tuple. Ordering on tuple is order first elements, if equal order on second element etc. Very easy to handle draws.

- `Action -> Float`, just sort on this Float. All attributes provide a number and take the product. Take for example, I want an aggressive bot so an offensive Action evaluates to 1.05, and a defensive action to 0.95. This way with many other attributes makes offensive Actions more likely.

Note: probably `State -> Action -> Float` is wanted because it is more likely you want to attack if you have few Planets left, and you want to defend more if you have an abundance in Planets.

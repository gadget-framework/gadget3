# Terms used in g3

## stock

A stock object, defined by g3s_* functions, configures the storage arrays for a
stock and how to iterate over a stock to get length groups.

## action

An action is defined by any of the g3a_* functions, for example g3a_age().

An action is a named list of steps (see later). When building a model these
lists will be combined and sorted alphanumerically to get a list of all steps
to be run on every timestep in the model. Currently the naming scheme for steps
is "step(nnn)", where nnn is a 3 character integer relating to that actions
position in the gadget order of computation.

## step

Individual steps are formula that define the code to be done at that steps. As
they are forumlae, they are associated to the function's enviromnent. What this
means they will stay aware of variables defined within the g3a_* function, and
these will be used to generate variable definitions.

For example, a snippet of g3a_time():

```
g3a_time <- function(start_year, end_year, steps = c(12)) {
    # Define an initial value for cur_time. If this variable gets used,
    # this will be used to define the variable in R and C++
    # Values may be inserted into an attached data object
    cur_time <- as.integer(0)

      . . .

    # Define a variable as another formulae, the initial definition
    # will be based on this. Any variables that it uses will get defined first.
    # NB: We refer to the function arguments here, the supplied arguments to
    # this function will get used to define them in the model source too.
    total_steps <- ~length(steps) * (end_year - start_year) + length(steps) - 1

    # Return a list of steps
    list(step0 = ~{
        # Here we use both definitions above
        if (cur_time > total_steps) return(nll)
    })
}    
```

We can see the results by calling ``g3_compile_r`` or ``g3_precompile_tmb``:

```
> writeLines(g3_precompile_tmb(g3a_time(1990, 1994)))
   . . .
template<class Type>
Type objective_function<Type>::operator() () {
    // NB: Since the initial value was an integer, we define the C++ type as integer
    int cur_time = 0;
    
    int steps = 12;
    // NB: These came from the function arguments
    int end_year = 1994;
    int start_year = 1990;
    auto total_steps = (steps).size()*(end_year - start_year) + (steps).size() - 1;
      . . .
    while (true) {
          if ( cur_time > total_steps ) return(nll);
    }
}
```

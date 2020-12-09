
# Code Journal for Thesis

Since writing the essay, the thesis is pivoted towards exploring external tools suggested language servers
that can be integrated with talon. This journal is an attempt at structuring my work from here on out.
Up until this point i have been experimenting with generating talon commands using tree sitter and pure script.
Starting next friday, i will have meetings with my supervisor every other week, so it is time to start producing.

## 20.10.20

Working on getting set up with latex.
I tried using petters thesis as a template, but was having problems with rubber.
After removing the completion of the documentation, i got it installed, but the project was still not compiled.
Furthermore, inotifywait is not supported on mac. 

Currently looking at the essay to see if i can keep using that.
I'm not sure if there's anything wrong with my current set up.
At some point i should figure out how to have multiple source files.

Goal for today is just to set up a thesis project and begin a rough outline.

## 21.10.20

Starting to work on that outline. Would also be nice if i could hook up some citations today.

Had to detour to rewriting the latex commands from the old api.
Finally figured out how to use list correctly in the new api.
There's no need for defining custom captures.
Guess i should have read the documentation.

Didn't do much on the outline.
After group session i read a bit from the NatLink paper.

## 22.10.20

Spent the morning writing the bekk article, followed by workout and study group, so no outline today either.

## 23.10.20

Slow day... gonna try to start on that outline. It's 13 pm already :(

Actually fleshed out the outline a bit. Starred a bunch of papers on google scholar, but haven't figured out how to cite them.

Figured out how to cite. Google scholar has a quotation button which can generate biblatex sources.
Also installed mendeley extension. Will see how much I end up using tha, but it looks like a nice system.
Added sections about lsp, tree sitter and elm.

Started looking at the sample paper i got from Yngve. Next step is to work their structure into my paper.

## 27.10.20

Yet another slow day. Group session at 12, spent time on HLS.

Got to writing a bit about methodology. Still a lot I'm unsure about, but covered a few ways of evaluating the system.

## 28.10

Before noon: got my herman miller, cleaned the desk and stated working on fitting my structure
to that of the example thesis. Wrote some notes on what to write in the motivation section.
Started looking into customizing th chapter header to make it like the one in the example, but
I have to figure out how to install tex packages.

## 29.10

Bit more structure and thoughts. Didn't do too much today. Send what I have to supervisor for the meeting tomorrow.

## 30.10

Managed to style the chapter headers :) Looks much nicer now.

Meeting notes:
- research question: can creating of voice commands be automated?
- figures!
- illustrate the workflow
- models?? client server?
- explain the methods
- present literature review in methods?
- project before method, then implementation

priorities:
problemstilling
explain the project!
background can be done later--

## 3.12

It looks like I haven't done anything since the last meeting, but at least I nailed the static analysis exam ;)

Currently working on my Haskell command set.
The current version of knausj has examples of declaring types and functions etc in dynamic lists for a given programming language.
I am now able to dictate Haskell types clearly. `either string hint` becomes `Either String Int`.
Even IO works. Abbreviations works, but each letter has to be capitalized and delimited by spaces: `"I O": "IO"`.
I implemented `code_insert_function` for Haskell, which is just inserting a space.
knausj encourages making general voice commands for all languages, but I'm currently not seeing the benefit of this abstraction.
A lot of the standard implementation for imperative languages does not make sense for ML style languages.
I might make an issue about this.

To capture multiple types I made a custom capture. The API is still under-documented, so I'm still not sure how to work with the capture value `m`.

Dictating function signatures is very smooth now.

## 6.12

Modules in Haskell commands.
Commands for importing and invoking function from that module.

## 8.12

Discovered that you don't need the `user.` prefix when referring to a capture in an action.
`import { user.haskell_module_list }: user.haskell_import_module(haskell_module_list)`

Currently don't know how to referenc the same capture in a rule.
One workaround is to duplicate the capture and give it a second name 
```Python
@mod.capture(rule="{self.haskell_type_list}+")
def haskell_type_(m) -> str: return str(m)

@mod.capture(rule="{self.haskell_type_list}+")
def haskell_type(m) -> str: return str(m)
```

Sent a message to Ryan.

## 9.12

Experimented with data first technique:
```
<user.text>:
    insert(user.formatted_text(text, "PRIVATE_CAMEL_CASE"))
    " "
```

This allows for dictating function calls without a command prefix. `map maybe -> mapMaybe `
This decreases the overall accuracy. 
When discussing this topic in the thesis I might use `approximation` to refer to how closely the set of phrases the system recognizes
correspondence with the set of possible program fragments with respect to what identifiers are defined (and how they are classified).
This would be an over-approximation.

Figured out how to reference a list from a cap tree with a multiplier:
```
test <user.haskell_module_alias>+:
    user.insert_many(haskell_module_alias_list)
```



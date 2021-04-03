
Outline:
- Introduction
- Preliminary questions
- User tries the system
- Impressions


# Introduction
This is where I introduce myself and the project.

Ask about how much they can type?

# Preliminary questions
Here I ask about the participants background and level of experience
both in programming in general and using talon/vocal programming.

- How long have you been programming?
- How long have you been using Talon or other Voice Coding systems?
- How much do you use Talon compared to keyboard and mouse in your day to day work?
- Which speech engine are you using?
- What command set are you using (is it baased on knausj)?

- Are you familiar with Elm or any similar languages such as Haskell or SML?
- Do you have experience using Vim?
If not , give a brief expanaition when showing the system.

Need to make sure they understand te following terms:
- Identifier
- AST / Node
- Structural editing

# User Testing

Explain the features of the system:
- Navigation (go [to|fun|type])
- Editing ([select|delete] [fun|type])
- Signature Dictation
- Function invocation

have the user try the example commands in the instructions.
here I will try to point out subtleties in the results:

- `go type/fun update`: the system distingushes based on node type, not on capitalization
- `go string/number`: not just matching on the quote symbol/number, see comments in Example.elm

This will be a form of black box testing.
I will give the user a list of commands and observe the action in the editor.

# Post Questions
Here I want to get their impression of the system and its potential.

- In general, what is your impression of what you have seen during this session?
- Were there any feature you thought was particularly useful?
- Were there any behaviour you found to be strange or unintuitive?
- Are there any features that are not implemented that you think could improve the system?
- Based on what you have seen so far, would you be interrested in using a finished version of a system like this?

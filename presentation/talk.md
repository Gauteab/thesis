---
author: Gaute Berge
title: Master Thesis 
date: June 16, 2021
---

## Leveraging Language Tooling for Better Voice Coding
Implementing program awareness and structural editing for Talon

# Content

- Motivation
- Background
- My Contribution

# Motivation

- RSI
- Other disabilities
- Under explored field with a lot of potential

# Talon

![](./images/talon_logo.png)

---

## Talon

- voice control
- noise control (pop/hiss)
- eye tracking
- facial gesture recognition
- full system control
- [demo](https://twitter.com/lunixbochs/status/1378159234861264896)

# Talon Files

- declare voice commands
- implement actions

---


<img src="./images/elmtalon.png" alt="image missing" height="650"/>

# Python Scripts

- modules
- captures
- actions
- lists

---

<img src="./images/elmpy.png" alt="image missing" height="650"/>

# LSP

The Language Server Protocol

---

![](./images/lsp-matrix.png)

---

![](./images/lsp-communication.png)

---

<img src="./images/coc.gif" alt="image missing" height="650"/>

# Tree Sitter

---

<img src="./images/tree-sitter-web.png" alt="image missing" height="650"/>

---

![](./images/playground.png)

---

```javascript
x = 5
```

```
(
    (assignment_expression 
        left: (identifier) @declared-variable) 
    (#eq? @declared-variable "x")
)
```

[playground](https://tree-sitter.github.io/tree-sitter/playground)

---

# Elm

<img src="./images/elm.png" alt="image missing" />

# My contribution

<a href="https://github.com/Gauteab/talon-tree-sitter-service" target="_blank">talon-tree-sitter-service</a>

# Goals

- Increase efficiency
- Make programming feel more natural

# Features

- Vocabulary generation
- High level navigation
- Structural editing

---

<img src="./images/running-exmple.png" alt="image missing" height="650"/>

---

<img src="./images/architecture.png" alt="image missing" height="650"/>

---

![](./images/rntz.png)

<a href="https://youtu.be/G8B71MbA9u4?t=2062" target="_blank">Programming language design & programming by voice! (LSD seminar, April 2021)</a>


# The Study

- semi-structured interview + usability test
- 6 participants
- remote

# Results

--- 

## Results From Prototype

- tree-sitter 👍
- lsp also has potential
- talon is easy to integrate with
- vim is great for plugin prototyping

--- 

## Results From User Study

- Very positive reception
- All users expressed some interest in using it
- Navigation was especially nice

# Miscellaneous

---

<img src="./images/haskell_pr.png" alt="image missing" height="650"/>

---

<img src="./images/purets.png" alt="image missing" height="650"/>

---

<img src="./images/spoken_form.png" alt="image missing" height="650"/>

# Conclusions

- interest ✅
- need ✅
- suitable tools ✅

Incredible potential!

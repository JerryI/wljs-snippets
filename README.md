# Snippets package for WLJS Frontend
**An extension for [WLJS-Editor](https://github.com/JerryI/wljs-editor) that adds snippets feature**

- evaluate functions
- insert / replace expression in the text
- process clipboard

Works only with **WLJS Frontend >= 0.9.8**, check [Releases](https://github.com/JerryI/wolfram-js-frontend/releases)

Use it with a hotkey combination `Win+P` or (`Cmd+P` on Mac)

![](screenshot.png)

Each snippet is a notebook, stored in `library/` folder, which is automatically scanned on startup. Each file contains special field, marked by `.export` and some other tags in the first line, will be pasted into the running notebook, when the snippet has been called.

For instance

*cell 1*
```markdown
    ## Title (used in search)
    Short description...
```

*cell 2*
```mathematica
.export
myVeryUsefulFunction[x_] := x // ToString
```

*cell 3*
```mathematica
.export
.html
<p>Other cell</p>
```

...

Then, if one will call this snippet from the searbar it will insert the cells marked with `.export`.

## Deep dive
There are many frontmatter directives you can use

### `.export`, `.exports`, `oncall-export`
Paste the content of the cells into the notebook

### `.evaluate`, `.oncall-evaluate`
Evaluate the content of the cell, when a user call the snippet from the menu

### `.evaluate-forreal`, `.oncall-evaluate-forreal`
Evaluate the content of the cell inside the notebook for real and removes it

### `.oncall-once-evaluate`
The same as previous, but does it once

### `.evaluate-export`, `.oncall-evaluate-export`
Evaluate the cell in the background and paste the result into the notebook

### `.export-inline`, `.oncall-export-inline`
Pastes the content into the text, where the cursor is located

### `.evaluate-insert`, `.oncall-evaluate-insert`
Evaluates the cell in the background and insers the result into the selected text or where the cursor is located

### `.onselected-evaluate`
Evaluates the cell in the background taking the selected cell as an argument. Please, use __anonymous__ function

### `.onselected-evaluate-export`
Evaluates the cell in the background taking the selected cell as an argument and paste the result into a new cell of the notebook. Please, use __anonymous__ function

### `.onselected-evaluate-replace`
Evaluates the cell in the background taking the selected cell as an argument and paste the result instead of the selected text. Please, use __anonymous__ function

### `.onclipboard-evaluate`
Takes the content of clipboard and applies the function from the cell on it. Please, use __anonymous__ function

### `.onclipboard-evaluate-export`
The same as the previous, but pastes the result into a new cell of the notebook. Please, use __anonymous__ function

### `.onclipboard-evaluate-insert`
Takes the content of clipboard and applies the function from the cell on it and inserts it into the cell, where the cursor is located. Please, use __anonymous__ function


## Contribution
Feel free to fork and add whatever you want to `library` folder, I will merge it to the master and everyone who are using WLJS Frontend will receive your snippet.

## License
Project is released under the GNU General Public License (GPL).

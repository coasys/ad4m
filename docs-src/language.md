# Creating AD4M Languages
COMING SOON

### Languages
For creating an expression we need to select a language that we create an expression in:
```js
const languages = await ad4mClient.languages.all()
const noteIpfsAddress = languages.find(l => l.name === 'note-ipfs').address
```
### Creating an Expression

```js
const exprAddress = await ad4mClient.expression.create("A new text note", noteIpfsAddress)
```
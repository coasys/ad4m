let setup = ({onResolve, onLoad}) => {
  onResolve({filter: /^https:\/\//}, resolveFile)
  onResolve({filter: /.*/, namespace: 'http-fetch'}, resolveUrl)
  onLoad({filter: /.*/, namespace: 'http-fetch'}, loadSource)
}

let resolveFile = ({path}) => {
  console.log('wow', path)
  return {
    path: path,
    namespace: 'http-fetch'
  }
}

export let resolveUrl = ({path, importer}) => {
  console.log('wow 2', path, importer)

  return {
    path: new URL(path, importer).href,
    namespace: 'http-fetch'
  }
}

export let loadSource = async ({path}) => {
  let source;
  if (path.includes('perspect3vism')) {
      const commit = path.split("#");

      const url = `${commit[0].replace("https://github.com", "https://raw.githubusercontent.com")}/${commit[1]}/lib/bundle.js`

      path = url;
  }

  console.log('test 1', path)

  source = await fetch(path)

  if (!source.ok) {
      let message = `GET ${path} failed: status ${source.status}`
      throw new Error(message)
  }

  let contents = await source.text()
  let pattern = /\/\/# sourceMappingURL=(\S+)/
  let match = contents.match(pattern)
  if (match) {
      let url = new URL(match[1], source.url)
      let dataurl = await loadMap(url)
      let comment = `//# sourceMappingURL=${dataurl}`
      contents = contents.replace(pattern, comment)
  }

  let {pathname} = new URL(source.url)
  let loader = pathname.match(/[^.]+$/)[0]

  return {contents, loader}
}

let loadMap = async url => {
  let map = await fetch(url)
  let type = map.headers.get('content-type').replace(/\s/g, '')
  let buffer = await map.arrayBuffer()
  let blob = new Blob([buffer], {type})
  let reader = new FileReader()
  return new Promise(cb => {
      reader.onload = e => cb(e.target.result)
      reader.readAsDataURL(blob)
  })
}

export default {
  name: "custom-http-fetch",
  setup
};
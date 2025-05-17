# Work Log

## Initial Minimum Elm Project

1. Create the project directory:
```
$ mkdir frontend && cd frontend
```

2. Initialize the Elm project:
```
$ elm init
$ tree
.
├── README.md
├── elm.json
└── src
```

3. Create `src/Main.elm`:
```
module Main exposing (main)

import Browser
import Html exposing (text)

main =
    Browser.sandbox
        { init = ()
        , update = \_ model -> model
        , view = \_ -> text "Hello, Elm!"
        }
```

4. Create `index.html`:
```
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8" />
    <title>Elm App</title>
  </head>
  <body>
    <div id="elm"></div>
    <script src="elm.js"></script>
    <script>
      var app = Elm.Main.init({ node: document.getElementById("elm") });
    </script>
  </body>
</html>
```

----

## How to Run & Test
- Build
```
$ cd ex54/frontend
$ elm make src/Main.elm --output=elm.js
```
- Start the development server:
```
$ npx serve -s
```

- Short URL Creation Page: `http://localhost:3000/ex54`  
- Stats Page: `http://localhost:3000/ex54/{{short URL slug}}/stats`
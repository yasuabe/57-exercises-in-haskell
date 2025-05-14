# MEMO
### Preparation
Start MongoDB in your local Docker environment with the command below:
```
$ docker run mongodb-local
```

## How to check for MongoDB updates
```
$ docker exec -it mongodb-local bash

root@c778ada30f16:/# mongosh

test> use text_sharing
switched to db text_sharing

text_sharing> db.snippets.find()
[
  {
    _id: ObjectId('680f1b0ab3571addd20fb356'),
    slug: '97573137ec0f063578016de7a54357b7',
    content: 'test snippet'
  },
  {
    _id: ObjectId('680f1b18b3571addd20fb357'),
    slug: '3bb9455cc088f19968e27c40807d833e',
    content: 'test snippet (edited)'
  }
]
text_sharing> 
```

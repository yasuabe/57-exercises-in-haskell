# Memo
### Preparation
Start Redis in your local environment with the command below:
```
$ docker run --name redis-local -d -p 6379:6379 redis
```

### How to Run
```
$ stack run ex53
>  :list
>  :add first task
>  :list
6) first task
>  :add second task
>  :list
6) first task
7) second task
>  :remove 6
>  :list
7) second task
>  :exit
Bye.
```

### How to view Redis data directly
The todo items are stored in a Redis hash with the key `ex53:tasks`, viewable using redis-cli.
```
127.0.0.1:6379> hgetall ex53:tasks 
1) "5"
2) "my todo 5"
```
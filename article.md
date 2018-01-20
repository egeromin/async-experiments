# Asynchronous Programming and Microservices: Comparing Javascript, Erlang and Python with RabbitMQ + Celery


*This article is about building asynchronous microservices. I'll compare how this can be achieved in Javascript and Erlang natively, and in Python using RabbitMQ and Celery.*


## But why?

My first encounter with asynchronous programming in python was when building a web backend. Upon completing a purchase, the user should eventually receive a PDF invoice by email. This didn't have to happen immediately during the request; in fact, it was better if it didn't, so as not to slow down the purchase needlessly. At the time I wasn't sure how to implement an asynchronous workflow in python, but a quick google search quickly lead me to [Celery](http://www.celeryproject.org/) and [RabbitMQ](https://www.rabbitmq.com/). Celery is very easy to use; the only pain is setting up a message broker -- RabbitMQ, in my case. Once you're set up, running a task in the background is as easy as writing, in `myapp.py`,

```python
from celery import Celery

app = Celery('myapp', broker='amqp://localhost:5672')
# create celery 'app', takes as input the URL to RabbitMQ

@app.task()
def my_function(value):
    print("Print {} asynchronously".format(value))

if __name__ == "__main__":
    my_function.delay("me")
    # runs print("Print me asynchronously") asynchronously!
```

...and starting the worker, using the command

```
celery -A myapp worker
```

Then running `python myapp.py` will run `my_function` asynchronously in the worker process you just started.

That's what I did, with my invoicing code instead of `my_function`, and off I went happily implementing the rest of the backend. However, it left me wondering *how* celery could and should be used. This I knew: that RabbitMQ implemented a queue where messages would be stored, and that the celery worker pulled and processed the messages in this queue one by one until they ran out. But how else can celery and rabbitmq be used? And how does asynchronous programming work in other languages and contexts?

## The Email Cruncher

I'll illustrate microservices and asynchronous programming in javascript, erlang and python using a simple example, the *email cruncher*. You can paste a confidential message into a text box and the email cruncher will notify you of how many emails it found in it!

![js-cruncher-screenshot](https://user-images.githubusercontent.com/14077124/35188157-20f7f3d0-fe31-11e7-91f3-358e6c8a19ca.png)

The code for it is here:

* https://github.com/egeromin/async-experiments

The architecture is very simple. There are 3 microservices:

* The *uploader*, which reads the data in a file or textbox and passes it on to
* The *cruncher*, which finds emails in the data by regex and counts them, passing the count on to
* The *status displayer*, which keeps a count of the total number of emails ever found and displays it to the user.

Each of these microservices should have their own 'mailbox', a queue for incoming messages, in order to process them one by one.

![Simple Email Cruncher](https://user-images.githubusercontent.com/14077124/35188168-5132dc9a-fe31-11e7-9b91-b831b4570bb8.png)

So, keep this example in mind as we move on :smile:


## Javascript

Javascript is an obvious example of asynchronous programming. Most modern websites make heavy use of javascript's AJAX, which stands for asynchronous javascript and xml. AJAX is a way for the user to perform a request to an external server without the website freezing -- a specific example of 'asynchronous I/O'. In code, this is implemented using *callbacks*. Instead of writing code that makes a request, waits for the response, and then does something with it, you instead write code that makes a request and specifies which *callback function* should be run when the request returns. Example:

```javascript
var xmlhttp = new XMLHttpRequest();

xmlhttp.onreadystatechange = function() {
    console.log("Execute some code");
};

xmlhttp.open("GET", "my_resource.html", true);
xmlhttp.send();
```

What does this do? We first set the callback function to be run when the `onreadystatechange` event fires, and then trigger the actual request. The callback function will be run when the request terminates and the `onreadystatechange` event is fired.

What's an event, though? An *event* is a useful javascript abstraction and it's up to the browser to implement it. All we need to know is that there's a whole range of 'events' we can attach callbacks to. Most importantly, these are: changes to the DOM because of user interaction, and responses from servers. Any such real-world event triggers a javascript event. The details of the implementation shouldn't concern us now -- instead, the interesting questions is, given that we may attach callback functions to events, how and when exactly are these callbacks run? Are they all run in the same process, or thread, and at what time? In what order are callbacks run once they are fired, and who is responsible for firing them?

As mentioned, events are a useful abstraction for 'something happening' in our browser or browser tab. The browser detects that, for example, a request has returned or that a user is hovering her mouse over a particular button. This corresponds to an *event*, with a particular name, such as `onchange` or `onmouseover`. If, in our javascript code, we've set a callback for one of these events, then the browser runs the corresponding code. So what happens if there's many events going on at the same time, or almost? What happens if while one callback is running, another event happens? Are they processed concurrently using threads, or one after another, and in which order?

It turns out that in javascript there is a *single message queue* where callbacks are enqueued. Any javascript code you write happens in the same thread. This thread implements an event loop: it waits for the next message in the queue, and then processes it.

```javascript
while (queue.waitForMessage()) {
  queue.processNextMessage();
}
```

In particular, whenever a javascript *event* happens, the corresponding callback gets added to the message queue, if there's a callback defined.

One implication of this is that no two callback functions you define are ever run concurrently. So say you define a function `modifyState` which increments some global state `counter`:

```javascript
var counter = 0;  // global variable

var mycallback = function(e) {
    console.log('Value of counter is: ' + counter.toString());
    counter += 1;
    console.log('Value of counter is: ' + counter.toString());
}

var mousebutton = document.getElementById("my-button");
mousebutton.addEventListener("mouseover", mycallback);
```

Then you can be sure that when you hover over `my-button`, the console will print N and then N+1, in order -- whatever else happens. More generally, your callback may assume that the state of your program won't be modified by any other callback.

### Multiple Runtimes

Each iframe in your HTML or javascript *worker* has its own message queue, stack and heap. That means that we can implement concurrent 'microservices' in javascript by spinning up new javascript workers.

```javascript
var my_worker = new Worker('my-worker.js') //spin up a new worker
// could also be an iframe, e.g.
// var my_iframe = document.getElementById('my-iframe');

var my_message = {message: 'MyMessage'}
my_worker.postMessage(my_message)  // post message to worker
```


### Email Cruncher in Javascript

Time for the email cruncher! We now know that each javascript runtime -- including the main script and each worker it spawns -- has its own message queue. So we can try to implement the email cruncher architecture using a separate worker for the 'cruncher' microservice. We'll have 2 microservices in total, each with its own queue:

* The main microservice, in 'main.js', responsible for reading 'uploaded' files and updating the DOM with the updated total number of emails found
* The cruncher microservice, in 'email_worker.js', which takes a string as input, finds emails in it by regex, and outputs the result.

I've implemented this in https://github.com/egeromin/async-experiments/tree/master/javascript . Check out the README for instructions with how to run.


The architecture is actually a little bit different than originally planned, as we 'merged' 2 microservices, the uploader and the status displayer. It looks like this instead:

![js-cruncher-flowchart](https://user-images.githubusercontent.com/14077124/35188170-55c086b8-fe31-11e7-8b35-0827794374bd.png)

The code for counting the total number of emails found is the callback to the `onmessage` event in `main.js`. It reads the current number of emails from the DOM, adds the number of new emails as declared in the message, and then updates the DOM. Note that this callback takes advantage of the fact that there's only ever 1 callback running at a time in javascript. Otherwise, this would not be a reliable way of updating the total number of emails, since the total in the DOM might have changed by a different instance of the callback in some other thread. However, again, this is not an issue as there's only ever one callback running at a time and so there are no race conditions.

Useful link:

* For more detailed information about the javascript event loop, check out this article:
  - https://developer.mozilla.org/en-US/docs/Web/JavaScript/EventLoop


## Erlang

Erlang is a language for programming highly concurrent and asynchronous applications. According to http://www.erlang.org,

> Erlang is a programming language used to build massively scalable soft real-time systems with requirements on high availability. Some of its uses are in telecoms, banking, e-commerce, computer telephony and instant messaging. Erlang's runtime system has built-in support for concurrency, distribution and fault tolerance.

The WhatsApp backend is [written in Erlang!](https://www.fastcompany.com/3026758/inside-erlang-the-rare-programming-language-behind-whatsapps-success)

Erlang has a number of interesting features:

* Single assigment. In a function, you can only assign a value to a variable once. This greatly simplifies thinking about state, since a variable's value cannot be changed, once it's been assigned.
* No for loops. Like many other functional programming languages, there are no for loops, so to implement loops you must use recursion. This forces you to think and design your program in terms of recursion.

The main reason I'm exploring it here though is that it's very easy to spawn new 'erlang subprocesses'. These are not real OS subprocesses, as erlang runs inside a virtual machine called BEAM. This VM uses threads, but not necessarily one thread per erlang subprocess, as it's the VM scheduling the tasks, not the underlying OS. Joe Armstrong, the creator of Erlang, explains Erlang processes:

- Everything is a process.
- Processes are strongly isolated.
- Process creation and destruction is a lightweight operation.
- Message passing is the only way for processes to interact.
- Processes have unique names.
- If you know the name of a process you can send it a message.
- Processes share no resources.
- Error handling is non-local.
- Processes do what they are supposed to do or fail.

See [Joe Armstrong's PhD Thesis](http://erlang.org/download/armstrong_thesis_2003.pdf) for more details.

So in particular, every Erlang subprocess has its own 'mailbox': a queue to which we can send messages, which can be any erlang object, or *term*.

```erlang
Message_pid ! {a_message, "Message string"},
% Send a message to the process with id `Message_pid`
```

### Email Cruncher in Erlang

What this means is that Erlang is perfect to write a version of our email cruncher! I've implemented a command line version of the email cruncher using erlang in  https://github.com/egeromin/async-experiments/tree/master/erlang . Do take a look :wink: . A few things worth noting:

* The microservices are immediately recognisable in the code: `prompt`, `emailExtractor` and `resultDisplayer`
* To implement an infinite loop, I must use recursion (in all of the 3 microservices)
* *Pattern matching* is used in order to determine which action I should perform in the microservice (only 2 options at the moment:`process` or `quit`). This idea of using atoms to mark the 'type' of a message is a common pattern in Erlang.
* Because of Erlang's single assignment, there is no way to store global state in a variable. To keep a running total in `resultDisplayer`, I must use recursion.


Useful links:

* An official introduction to Erlang on the Erlang website: http://erlang.org/doc/reference_manual/users_guide.html


## Interlude: AMQP and RabbitMQ

Now that we've explored async programming and microservices in javascript and erlang, which support message passing and queues natively, what about other languages, such as python, which do not have this feature?

One solution is to use a *message broker*. Message brokers are software specifically for *sending messages* between different processes / programs. Instead of sending messages to other processes over sockets or pipes directly, you send them to the broker, which acts as an intermediary. The benefit of using a message broker is that you no longer have to worry about the details of sending messages, such as queuing and routing, and can delegate these tasks to the broker. For example, you can instruct the broker to send a message to a particular queue or set of queues. Usually message brokers are implemented as servers with which you can interact over TCP using a custom protocol.

There are many different message brokers, and I'll limit myself to briefly discussing 1 only: RabbitMQ, which implements AMQP, the "Advanced Message Queuing Protocol". Your best introduction to this is [RabbitMQ's excellent set of tutorials](https://www.rabbitmq.com/getstarted.html), which I warmly recommend reading. Here's a summary:

### Queues, Producers and Consumers

RabbitMQ implements queues. New queues must be *declared* using an AMQP API call. Queues may be identified using a *binding key,* which is used for routing using exchanges (more below). *Producers* can push messages to a queue, and *consumers* may consume them one at a time. Both producing and consuming correspond to specific actions in the AMQP protocol. Queues are stored in memory, but can be persisted to disk, if they are appropriately declared.

### Exchanges

In reality, producers don't publish directly to queues. Instead, they publish to *exchanges*, which are responsible for routing. Exchanges differ from queues in that they have no memory. Their role is just to route on messages to queues based on the message's *routing key*. If there are no queues attached to a particular exchange, then any messages published to that exchange will simply be lost.

In AMQP, there are 3 main types of exchanges:

* *fanout* - the message is passed on to every queue attached to that exchange
* *direct* - the message is passed on to the queue whose *binding key* matches the message's *routing key* exactly
* *topic* - the message is passed on to all of the queues whose binding keys' pattern match the message's routing key. For example, the pattern `media.*` matches both `media.video` and `media.audio`.

Exchanges allow us to implement [*publish/subscribe*](https://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern), in which a number of different services *subscribe* to a particular exchange and choose to receive messages from it, either selectively, in the case of direct and topic exchanges, or indiscriminately, as is the case with fanout exchanges.


### The Prefetching Quirk

By default, a queue 'offloads' its messages to every consumer currently subscribed to it in a round-robin way -- *before* the consumer has finished processing the consumed task! This is because messages are 'dispatched' to consumers immediately, which means that they're reserved for that particular consumer, even if it hasn't finishing processing the message yet. While this ensures that each consumer gets approximately the same number of tasks, it can be a problem if some tasks take much longer to process than others. In that case, although the number of *messages* received by each worker will be approximately the same, some workers might end up doing much more work than others. To ensure that a worker receives a new message only as soon as it's ready, you must [disable prefeching](https://www.rabbitmq.com/tutorials/tutorial-two-python.html).


### RPC

RabbitMQ can be used to perform synchronous calls using RPC, which stands for remote procedure calls. The idea is to publish a task to a specific task and specify a *reply-to queue*, which is the queue that the result should be sent to. The caller should then wait for the result by consuming from this reply-to queue. To see how this is implemented in python using pika, check out [this tutorial](https://www.rabbitmq.com/tutorials/tutorial-six-python.html).


### The actual protocol

One of the things I was wondering while exploring AMQP is, 'what does the actual protocol look like?' Both HTTP and SMTP for example are relatively simple text-based protocols. RabbitMQ is a bit more complicated. If you're feeling brave, dive straight into the [official specification](https://www.rabbitmq.com/resources/specs/amqp0-9-1.pdf)! Admittedly I didn't read all of this, but I think I get the gist:

* AMQP is a binary protocol. Binary data is sent over TCP in units called *frames*
* There are 2 parts to the specification:
    * a 'functional layer' which defines the functionality to be supported in terms of classes and methods. For example, the specification defines a  `Queue.Declare` method for declaring queues
    * a 'transport layer' which defines how to convert these method calls into binary data frames.

For a concrete implementation, check out how the spec is implemented in python by pika: https://github.com/pika/pika/blob/master/pika/spec.py


## AMQP in Python using Kombu

I'll be using [Kombu](http://kombu.readthedocs.io/en/latest/) to use RabbitMQ in python. Despite the RabbitMQ team recommending [pika](http://pika.readthedocs.io/en/0.11.2/index.html), I chose Kombu instead, because it's used by Celery. However, some AMQP features available in pika don't appear to be available in Kombu. For example, I couldn't find a way to have the RabbitMQ server pick a random queue name, which is possible in pika by declaring a queue without a name: `channel.queue_declare()`. Having given that warning, here's a very quick intro to Kombu

Example of declaring a direct exchange and publishing to that exchange

```python
from kombu import Connection, Exchange

if __name__ == "__main__":
    rabbit_url = "amqp://username:password@localhost:5778/vhost"
    exchange = Exchange('my-exchange', type='direct')
    with Connection(rabbit_url) as connection:
        producer = connection.Producer()
        producer.publish("some message!", exchange=email_exchange,
                         declare=[email_exchange], retry=True)
```

Example of declaring an exchange, a queue, and consuming from that queue:


```python
from kombu import Connection, Exchange, Queue

def message_callback(content, message):
    print("Message content: {}".format(content))  
    # callback to run when we receive a message

if __name__ == "__main__":
    rabbit_url = "amqp://username:password@localhost:5778/vhost"
    with Connection(rabbit_url) as connection:
        exchange = Exchange('my-exchange', type='direct')
        queue = Queue('my-queue', exchange=exchange)
        with Consumer(
            connection,
            queues=[queue],
            callbacks=[message_callback],
            prefetch_count=1  # disable prefetching
        ):
            while True:  # start consuming
                connection.drain_events()
                # `drain_events` blocks until there's a message in the queue,
                # and then consumes 1 message only.
```

### Updated Email Cruncher in Python using Kombu

Now that we know how to use RabbitMQ using Kombu, we can architect a slightly more complicated email cruncher with additional microservices.

Here's a flowchart:

![Full Email Cruncher](https://user-images.githubusercontent.com/14077124/35188167-4eb1e970-fe31-11e7-8395-8c73e637b9c9.png)

Here are our new microservices:

* The *prompt*. This is as before, except that it's the filename that's passed on to the crunchers, as opposed to the contents themselves. The filename is sent as a message to the *email exchange*, a fanout exchange
* *Fast and slow email crunchers*. Now there are 2 email crunchers, one 'fast' using regexes, and one 'slow' one using a slightly more advanced algorithm. These find emails in the original file and each pass a list of newly found emails on to the *status queue* (via the default exchange).
* The *status displayer*. This status displayer keeps track of all emails we found corresponding to a particular domain, say `spam_domain`. It maintains a global in-memory datastore of all previously found emails. If it finds any new ones, for each new one it sends a new message to the *spammer exchange*, a direct exchange. The content of the message is 'spam me!' and its routing key should be equal to the *local part* of the email address, i.e. the part before the '@'.
* The *spammer*. The spammer should be started by specifiying the binding key and name of the queue that it should consume from. For example, if it is started with binding key 'emanuel', it starts consuming from the 'emanuel' queue with binding key 'emanuel'. This queue is attached to the 'spammer' exchange. If the spammer receives a message from the queue, it sends a warning email to 'emanuel@spam_domain'

This example is admittedly a bit contrived. For example, the spammer microservice could be implemented much more simply by taking as input an email address and spamming it. However, this example illustrates the publish/subscribe in 2 different cases:

1. New 'email crunchers' can be added by attaching a new queue to the email exchange and consuming from it. For example, if we find yet another alternative algorithm to extract emails, we can add it as a new 'module' quite easily
2. If any user with a `spam_domain` email doesn't want to receive any warning emails, we can simply 'unsubscribe' her by stopping the corresponding instance of the spammer.

I've implemented a version of this in python using the Kombu python library here: https://github.com/egeromin/async-experiments/tree/master/python/kombu_version . Do take a look!

(*Painful admission*: The 'slow' cruncher is at the moment identical identical to the 'regex' cruncher, except that it sleeps 5 seconds for every message. One day I'll implement a proper alternative!)


## Using Celery instead

A quick look at the Kombu version of this extended email cruncher shows that there's a fair amount of boilerplate code. Each time, we have to connect manually, setup the consumer and producer, declare the queues, etc. This is where Celery comes in: it does much of the heavy lifting and allows you to directly define *tasks* inside your code which should be run asynchronously. I gave an example at the beginning of the article.

This means that instead of sending messages to exchanges, in celery you *invoke tasks asynchronously*. Under the hood, celery still sends 'text' messages over AMQP. It serializes a dictionary as JSON containing information that allows it to 'find' the right task on the other end. One big difference between regular AMQP and celery is that publish/subscribe is not possible using celery. This is because you invoke *individual tasks* using celery, rather than publishing to exchanges. **Celery is not a replacement or simplification of regular AMQP -- it is a different way of doing asynchronous programming**.

Here's the simplified architecture of the cruncher for celery:

![Celery Cruncher](https://user-images.githubusercontent.com/14077124/35188166-4d0bdac2-fe31-11e7-9d81-1f10d1750bda.png)

I've implemented a version of this in https://github.com/egeromin/async-experiments/tree/master/python/celery_version . Take a look at it -- in particular the README. I'll wait :wink:

A few things to note:

* We can create a pipeline of tasks using celery's `chain` function. This is part of [celery's Canvas](http://docs.celeryproject.org/en/latest/userguide/canvas.html), a bunch of functions for writing complex workflows. Other than `chain`, which concatenates tasks, there's also `group`, which runs a bunch of tasks in parallel, and `chord`, which runs tasks in parallel, waits for them to complete, and passes the list of all results on to a single callback function.
* The `chain` pattern breaks down when we invoke the spammer for each new spammable email -- there's no `Canvas` function that takes as argument a list and applies a function asynchronously to each element in the list. (`map` and `xmap` are different -- they invoke a single asynchronous task that sequentially applies a function to each element in the list, and then returns the result).
* By default, invoking a worker using `celery -A cruncher worker` consumes from *all* queues we declared. If, like in the kombu version, we want to run separate workers for each microservice, we have to restrict the queues the worker consumes from. For example, to start the cruncher, we have to run `celery -A cruncher worker -Q crunch`
*  The status microservice uses a global variable. We have to be careful, because invoking `celery worker` actually starts 4 subprocesses, each consuming from the queues -- so effectively 4 workers at once. We want to make sure it's only 1 worker and 1 in-memory global variable 'database'. We can do this by starting the status worker with concurrency 1: `celery -A cruncher worker -Q status -c 1`
* In the kombu version, the spammer is initialised with the local name it's supposed to spam. Here, because I cannot pass arguments to a celery worker, I use environment variables instead -- hence the `os.getenv("SPAMMER_QUEUE", 'spammer')` in `cruncher.py`.

Nevertheless, despite not being able to implement the full version of my very contrived email cruncher example in Celery, it remains hugely useful. As already shown, invoking tasks asynchronously is very easy. Creating complex pipelines of tasks is very easy using Canvas. And finally, RPC is available by default:

```python
@app.task
def add_one(x):
    return x + 1

res = add_one.delay(1)  # returns a 'promise'
result = res.get()  # blocks until the RPC returns
print(result)  # prints '2'
```

A really useful tool for monitoring Celery is [flower](http://flower.readthedocs.io/en/latest/). It's very easy to set up and comes with a nice interface for seeing which tasks have succeeded, failed, etc.

![Flower Screenshot](https://raw.github.com/mher/flower/master/docs/screenshots/tasks.png)


## What I didn't cover

There's many more things to asynchronous programming in python and AMQP.

* [asyncio](https://docs.python.org/3/library/asyncio.html) - an alternative to message brokers for asynchronous programming in python. It implements an event loop.
* Priority queues. AMQP allows you to define priority queues with priority 1-10. Messages with a higher priority get dispatched to workers first.

```python
# priority using celery
queue = Queue('my-queue', queue_arguments={'x-max-priority': 10})  # define a priority queue
task.apply_async({kwarg: "some kwarg"}, priority=4)  # invoke an async task with priority
```

* Other message brokers such as [Redis](https://redis.io/).
* More AMQP details such as heartbeats, etc.

That's all -- thanks for reading!

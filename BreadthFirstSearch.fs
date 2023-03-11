namespace Search

module BFS =
    let strategy = {
        empty = Queue.empty
        insert = Queue.enqueue
        remove = Queue.dequeue
    }

    let key n = n.state
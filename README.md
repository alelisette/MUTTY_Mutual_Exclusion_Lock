# <span style="font-size: 2em;">MUTTY_Mutual_Exclusion_Lock</span>

Implementation of a mutual exclusion using the Maekawa’s voting algorithm to coordinate processes when they access critical sections.

### <u><span style="font-size: 1.5em;">-lock1.erl</span></u>
This is a basic lock that prevents multiple workers from entering the critical section simultaneously.
Each lock instance has a unique identifier and can communicate with other instances to request access. When a request is received from another instance, it responds with an "ok" message, indicating approval. This lock does not implement deadlock prevention or fairness mechanisms, so it may result in conflicts and unequal access under high contention.

### <u><span style="font-size: 1.5em;">-lock2.erl</span></u>
This version introduces priority to manage conflicts and prevent deadlock. Each instance of lock2 has a unique identifier, which it uses as a priority level (lower values mean higher priority). When a lock instance receives a request from another instance, it checks the priority. If the other instance has a higher priority, it immediately grants access by sending an "ok" message. This prioritization helps manage conflicts but can lead to starvation for lower-priority instances.

### <u><span style="font-size: 1.5em;">-lock3.erl</span></u>
This lock uses Lamport clocks to provide fair access based on logical timestamps. Each lock instance maintains a Lamport clock, which is incremented with each access request. Requests are prioritized based on their Lamport timestamp, with earlier logical times gaining access first. If two requests have the same timestamp, priority is determined by the instance identifier. This logical ordering helps ensure fairness and avoids deadlocks, even in high-conflict scenarios. However, since workers themselves are not involved in the Lamport clock, real-time ordering is not guaranteed.

### <u><span style="font-size: 1.5em;">-Module or Mutty_new.erl</span></u>
The muty module sets up and manages the worker and lock instances. It can initiate lock instances using different lock modules (lock1, lock2, or lock3), which allows for flexibility in testing different locking mechanisms.
It also manages worker behavior, including sleep and work times, which are used to simulate varying levels of contention. The stop function terminates all worker instances, allowing the system to gather statistics and analyze the performance and fairness of each lock implementation under different configurations.

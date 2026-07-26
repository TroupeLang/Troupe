/** A FIFO queue with amortized O(1) enqueue and dequeue.
 *
 *  Backed by an array and a head index: dequeue advances the head instead
 *  of shifting the array (Array.prototype.shift re-indexes every remaining
 *  element, making queue draining quadratic). Dequeued slots are cleared so
 *  the queue does not retain references to consumed elements; the backing
 *  array is compacted once the consumed prefix is both large and at least
 *  half of the array, keeping compaction cost amortized O(1) per element.
 *
 *  clear() empties the queue in place, so the queue object can be safely
 *  aliased (e.g. a loop holding a reference across a reset).
 */
export class FifoQueue<T> {
    private items: (T | null)[] = [];
    private head = 0;

    /** Consumed-prefix length at which compaction is considered. */
    private static readonly COMPACTION_THRESHOLD = 4096;

    get size(): number {
        return this.items.length - this.head;
    }

    get isEmpty(): boolean {
        return this.head >= this.items.length;
    }

    enqueue(x: T): void {
        this.items.push(x);
    }

    /** Remove and return the oldest element, or undefined when empty. */
    dequeue(): T | undefined {
        if (this.isEmpty) {
            return undefined;
        }
        const x = this.items[this.head] as T;
        this.items[this.head] = null;
        this.head++;
        if (this.head >= this.items.length) {
            this.items.length = 0;
            this.head = 0;
        } else if (this.head >= FifoQueue.COMPACTION_THRESHOLD
                   && this.head * 2 >= this.items.length) {
            this.items.splice(0, this.head);
            this.head = 0;
        }
        return x;
    }

    clear(): void {
        this.items.length = 0;
        this.head = 0;
    }
}

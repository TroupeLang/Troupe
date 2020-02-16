const FIFO = require('p-fifo')
const defer = require('p-defer')
module.exports = function Pushable () {
  const errorPromise = defer()
  const returnPromise = defer()
  const fifo = new FIFO()

  return {
    [Symbol.asyncIterator] () {
      return this
    },
    next: () => Promise.race([
      errorPromise.promise,
      returnPromise.promise,
      fifo.shift()
    ]),
    return: async () => {
      returnPromise.resolve({ done: true })
      return { done: true }
    },
    throw: async err => {
      errorPromise.reject(err)
      return { done: true }
    },
    push: value => fifo.push({ value }),
    end: async err => {
      if (err) {
        errorPromise.reject(err)
        return
      }
      return fifo.push({ done: true })
    }
  }
}
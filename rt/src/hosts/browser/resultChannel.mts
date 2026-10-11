// The browser's result channel: lifecycle messages posted to the page.
//
// The channel is always open, since the page that started the worker is
// there to receive. Whether the page consumes the result -- enabled() --
// is its own statement, made in the run request: a page that does not is
// served the console copy of the main thread's result, as a process with
// no result socket prints it.
//
// The final message ends the worker. A process ends when its event loop
// drains after cleanup; a worker never does by itself, so the channel
// closes it once the message is posted and the cleanup that follows the
// message has run (cleanupAsync continues synchronously after sendAndClose
// resolves, so the close is queued behind it as a task).

import { ResultChannel, ResultMessage } from '../../ResultChannel.mjs';

export function browserResultChannel(
    post: (msg: ResultMessage) => void,
    supervised: boolean,
    close: (exitCode: number) => void,
): ResultChannel {
    return {
        enabled: () => supervised,
        connect: async () => {},
        send: async (msg) => { post(msg); },
        sendAndClose: async (msg) => {
            post(msg);
            setTimeout(() => close(msg.exitCode ?? 0), 0);
        },
    };
}

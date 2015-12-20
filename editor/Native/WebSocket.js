Elm.Native.WebSocket = {};
Elm.Native.WebSocket.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.WebSocket = localRuntime.Native.WebSocket || {};
    if (localRuntime.Native.WebSocket.values){
        return localRuntime.Native.WebSocket.values;
    }

    var Signal = Elm.Native.Signal.make (localRuntime);
    var Task = Elm.Native.Task.make (localRuntime);
    var Utils = Elm.Native.Utils.make (localRuntime);

    function create(url, address)
    {
        return Task.asyncFunction(function(callback) {
            var socket = new WebSocket(url);
            socket.addEventListener("message", function(evt) {
                // TODO: apparently this can be something other than a string
                // https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent
                Task.perform(address._0({
                    ctor: "Message",
                    _0: evt.data
                }));
            });
            socket.addEventListener("close", function(evt) {
                Task.perform(address._0({ctor: "Close"}));
            });
            socket.addEventListener("open", function(evt) {
                callback(Task.succeed(socket));
            });
        });
    }

    function send(socket, message)
    {
        return Task.asyncFunction(function(callback) {
            if(socket.readyState == WebSocket.OPEN) {
                socket.send(message);
                callback(Task.succeed(Utils.Tuple0));
            } else {
                callback(Task.fail({ctor: 'SocketNotOpenError'}))
            }
        });
    }

    function close(socket)
    {
        return Task.asyncFunction(function(callback) {
            socket.close();
            callback(Task.succeed(Utils.Tuple0));
        });
    }

    localRuntime.Native.WebSocket.values = {
        create: F2(create),
        send: F2(send),
        close: close
    };
    return localRuntime.Native.WebSocket.values;
};

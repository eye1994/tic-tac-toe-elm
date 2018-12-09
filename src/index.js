import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var node = document.getElementById("root");
var app = Elm.Main.init({node: node});
var ws = new WebSocket("ws://localhost:8080/ws");

ws.onmessage = function(message) {
    console.log(`elm-outgoing: ${message}`);
    app.ports.websocketIn.send(message.data);
};

app.ports.websocketOut.subscribe(function(msg) { 
    console.log(`elm-incoming: ${msg}`);
    ws.send(msg); 
});

registerServiceWorker();

import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var node = document.getElementById("root");
var host = location.href;
var urlParams = new URLSearchParams(window.location.search);
var app = Elm.Main.init({node: node, flags: { 
    host: host,
    joiningRoom: !!urlParams.get("room"),
    roomId: urlParams.get("room") || "",
    oponentName: urlParams.get("playerName") || "",
}});
var ws = new WebSocket("wss://game-server.sebestyen.me/ws");

ws.onmessage = function(message) {
    console.log(`elm-outgoing: ${message}`);
    app.ports.websocketIn.send(message.data);
};

app.ports.websocketOut.subscribe(function(msg) { 
    console.log(`elm-incoming: ${msg}`);
    ws.send(msg); 
});

app.ports.copy.subscribe((str) => {
  const el = document.createElement('textarea');
  el.value = str;
  document.body.appendChild(el);
  el.select();
  document.execCommand('copy');
  document.body.removeChild(el);
});

registerServiceWorker();

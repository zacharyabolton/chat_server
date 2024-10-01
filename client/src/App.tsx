// src/App.tsx
import React, { useContext, useState } from 'react';
import Messages from './components/Messages/Messages';
import { WebSocketContext } from './WebSocketContext';
import './App.css';

const App: React.FC = () => {
  const { sendMessage, socket } = useContext(WebSocketContext);
  const [input, setInput] = useState('');

  const handleSend = () => {
    sendMessage(input);
    setInput('');
  };

  const connectionStatus = socket
    ? socket.readyState === WebSocket.OPEN
      ? 'Connected'
      : 'Connecting...'
    : 'Disconnected';

  return (
    <div className='App'>
      <header className="App-header">
        <h1 className='App-title'>WebSocket Chat</h1>
        <p>Status: {connectionStatus}</p>
        <input
          className="App-input"
          type="text"
          value={input}
          onChange={(e) => setInput(e.target.value)}
          placeholder="Enter message"
        />
        <button
          className="App-send"
          onClick={handleSend}
          disabled={!socket || socket.readyState !== WebSocket.OPEN}
        >
          Send
        </button>
        <Messages /> {/* Render the Messages component */}
      </header>
    </div>
  );
};

export default App;

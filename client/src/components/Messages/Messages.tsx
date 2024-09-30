// src/Messages.tsx
import React, { useContext } from 'react';
import { WebSocketContext } from '../../WebSocketContext';

const Messages: React.FC = () => {
  const { messages } = useContext(WebSocketContext);

  return (
    <div>
      <h2>Messages:</h2>
      <ul>
        {messages.map((msg, idx) => (
          <li key={idx}>{msg}</li>
        ))}
      </ul>
    </div>
  );
};

export default Messages;

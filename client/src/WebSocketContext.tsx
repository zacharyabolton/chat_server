import React, { createContext, useEffect, useRef, useState, ReactNode } from 'react';

interface IWebSocketContext {
  socket: WebSocket | null;
  sendMessage: (message: string) => void;
  messages: string[];
}

export const WebSocketContext = createContext<IWebSocketContext>({
  socket: null,
  sendMessage: () => {},
  messages: [],
});

export const WebSocketProvider = ({ children }: { children: ReactNode }) => {
  const [socket, setSocket] = useState<WebSocket | null>(null);
  const [messages, setMessages] = useState<string[]>([]);
  const socketRef = useRef<WebSocket | null>(null);

  useEffect(() => {
    let wsUrl = '';
    if (process.env.NODE_ENV === 'development') {
      // In development, connect directly to the backend on port 8080
      wsUrl = 'ws://localhost:8080/ws';
    } else {
      // In production, use the same host and protocol as the current page
      const protocol = window.location.protocol === 'https:' ? 'wss' : 'ws';
      const hostname = window.location.hostname;
      const port = window.location.port ? `:${window.location.port}` : '';
      wsUrl = `${protocol}://${hostname}${port}/ws`;
    }

    console.log(`Connecting to WebSocket at: ${wsUrl}`);
    const ws = new WebSocket(wsUrl);
    socketRef.current = ws;
    setSocket(ws);

    ws.onopen = () => {
      console.log('WebSocket connection opened');
    };

    ws.onmessage = (event) => {
      console.log('Received:', event.data);
      setMessages((prev) => [...prev, event.data]);
    };

    ws.onclose = () => {
      console.log('WebSocket connection closed');
    };

    ws.onerror = (error) => {
      console.error('WebSocket error:', error);
    };

    return () => {
      ws.close();
    };
  }, []);

  const sendMessage = (message: string) => {
    if (socketRef.current && socketRef.current.readyState === WebSocket.OPEN) {
      socketRef.current.send(message);
    } else {
      console.error('WebSocket is not open');
    }
  };

  return (
    <WebSocketContext.Provider value={{ socket, sendMessage, messages }}>
      {children}
    </WebSocketContext.Provider>
  );
};

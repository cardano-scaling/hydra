// Buffer polyfill - must be at the very top before any other imports
import { Buffer } from 'buffer';

// Make Buffer globally available
if (typeof window !== 'undefined') {
  (window as any).Buffer = Buffer;
  (window as any).global = window;
}

// Polyfill process for some libraries
if (typeof (window as any).process === 'undefined') {
  (window as any).process = { env: {} };
}

import React from 'react';
import ReactDOM from 'react-dom/client';
import App from './App';
import './index.css';

ReactDOM.createRoot(document.getElementById('root') as HTMLElement).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
);

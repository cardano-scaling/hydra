import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import wasm from 'vite-plugin-wasm'
import topLevelAwait from 'vite-plugin-top-level-await'

// https://vitejs.dev/config/
export default defineConfig({
    plugins: [
        react(),
        wasm(),
        topLevelAwait()
    ],
    define: {
        'global': 'globalThis',
    },
    resolve: {
        alias: {
            buffer: 'buffer',
        }
    },
    optimizeDeps: {
        include: ['buffer'],
        esbuildOptions: {
            define: {
                global: 'globalThis'
            }
        }
    },
    server: {
        // Proxy requests to hydra-node to avoid CORS issues
        proxy: {
            '/hydra-api': {
                target: 'http://localhost:4001',
                changeOrigin: true,
                rewrite: (path) => path.replace(/^\/hydra-api/, ''),
                configure: (proxy, _options) => {
                    proxy.on('error', (err, _req, _res) => {
                        console.log('Proxy error:', err);
                    });
                    proxy.on('proxyReq', (proxyReq, req, _res) => {
                        console.log('Proxying:', req.method, req.url, '->', proxyReq.path);
                    });
                    proxy.on('proxyRes', (proxyRes, req, _res) => {
                        console.log('Proxy response:', proxyRes.statusCode, req.url);
                    });
                },
            }
        }
    }
})

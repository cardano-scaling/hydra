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
        // Supports dynamic ports: /hydra-api/4001/commit -> http://localhost:4001/commit
        proxy: {
            '/hydra-api': {
                target: 'http://localhost:4001', // Fallback, router overrides this
                changeOrigin: true,
                router: (req) => {
                    // Extract port from path: /hydra-api/4001/commit -> 4001
                    const match = req.url?.match(/^\/hydra-api\/(\d+)/);
                    if (match) {
                        const port = match[1];
                        const target = `http://localhost:${port}`;
                        console.log(`[Router] Detected port ${port} in URL, routing to ${target}`);
                        return target;
                    }
                    console.log(`[Router] No port in URL "${req.url}", using default 4001`);
                    return 'http://localhost:4001';
                },
                rewrite: (path) => {
                    const rewritten = path.replace(/^\/hydra-api\/\d+/, '');
                    console.log(`[Rewrite] ${path} -> ${rewritten}`);
                    return rewritten;
                },
                configure: (proxy, _options) => {
                    proxy.on('error', (err, _req, _res) => {
                        console.log('[Proxy Error]:', err.message);
                    });
                    proxy.on('proxyReq', (proxyReq, req, _res) => {
                        console.log(`[Proxy] ${req.method} ${req.url} -> ${proxyReq.protocol}//${proxyReq.host}${proxyReq.path}`);
                    });
                    proxy.on('proxyRes', (proxyRes, req, _res) => {
                        console.log(`[Proxy Response] ${proxyRes.statusCode} for ${req.url}`);
                    });
                },
            }
        }
    }
})

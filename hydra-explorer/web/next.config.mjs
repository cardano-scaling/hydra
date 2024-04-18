/** @type {import('next').NextConfig} */
const nextConfig = {
  output: 'export',
  env: {
    NETWORK_URL: process.env.NETWORK_URL
  },
};

export default nextConfig;

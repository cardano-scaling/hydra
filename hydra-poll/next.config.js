/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  env: {
    REACT_APP_HYDRA_NODE_URL: process.env.REACT_APP_HYDRA_NODE_URL,
    PLUTUS_SCRIPT_CODE: process.env.PLUTUS_SCRIPT_CODE
  },
  webpack: function (config, options) {
    config.experiments = {
      asyncWebAssembly: true,
    }
    return config
  },
}
module.exports = nextConfig

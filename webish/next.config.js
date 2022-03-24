/** @type {import('next').NextConfig} */

const nextConfig = {
  reactStrictMode: true,
  async rewrites() {
    return {
      fallback: [
        {
          source: '/_tscalibs/:path*',
          destination: 'http://127.0.0.1:8090/_tscalibs/:path*' // Proxy to Backend
        },
        {
          source: '/_tscadev/:path*',
          destination: 'http://127.0.0.1:8090/_tscadev/:path*' // Proxy to Backend
        },
        {
          source: '/api~003/:path*',
          destination: 'http://127.0.0.1:8090/api~003/:path*' // Proxy to Backend
        },
        {
          source: '/widgets/o/:path*',
          destination: '/widgets/:path*' // Reusing same implementation
        },
        {
          source: '/widgets/o/:path*',
          destination: '/widgets/:path*' // Reusing same implementation
        },
      ]
    }
  }
}

module.exports = nextConfig

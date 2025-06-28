// Minimal service worker that immediately unregisters itself
// This helps clear any cached Gatsby service worker

self.addEventListener('install', function(event) {
  console.log('Cleanup service worker installing...');
  // Skip waiting to activate immediately
  self.skipWaiting();
});

self.addEventListener('activate', function(event) {
  console.log('Cleanup service worker activating...');
  
  event.waitUntil(
    // Clear all caches
    caches.keys().then(function(cacheNames) {
      return Promise.all(
        cacheNames.map(function(cacheName) {
          console.log('Deleting cache:', cacheName);
          return caches.delete(cacheName);
        })
      );
    }).then(function() {
      // Unregister this service worker
      console.log('Unregistering cleanup service worker...');
      return self.registration.unregister();
    }).then(function() {
      // Claim all clients to ensure they get the message
      return self.clients.claim();
    }).then(function() {
      // Tell all clients to reload
      return self.clients.matchAll().then(function(clients) {
        clients.forEach(function(client) {
          client.postMessage({ type: 'RELOAD_PAGE' });
        });
      });
    })
  );
});

// Handle any fetch requests by going directly to network
self.addEventListener('fetch', function(event) {
  event.respondWith(fetch(event.request));
});
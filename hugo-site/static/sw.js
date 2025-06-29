// This file exists only to handle requests from cached Gatsby service workers
// It should not be actively registered by the Hugo site

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
      if (cacheNames.length === 0) {
        console.log('No caches to clear');
        return Promise.resolve();
      }
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
    })
  );
});

// Handle any fetch requests by going directly to network
self.addEventListener('fetch', function(event) {
  event.respondWith(fetch(event.request));
});
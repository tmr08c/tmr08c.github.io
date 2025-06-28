// Service Worker Cleanup Script
// This script unregisters any existing service workers and clears caches
// to fix issues from the previous Gatsby site

(function() {
  'use strict';

  // Unregister all service workers
  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.getRegistrations().then(function(registrations) {
      for (let registration of registrations) {
        console.log('Unregistering service worker:', registration.scope);
        registration.unregister();
      }
    }).catch(function(error) {
      console.log('Service worker unregistration failed:', error);
    });
  }

  // Clear all caches
  if ('caches' in window) {
    caches.keys().then(function(cacheNames) {
      return Promise.all(
        cacheNames.map(function(cacheName) {
          console.log('Deleting cache:', cacheName);
          return caches.delete(cacheName);
        })
      );
    }).then(function() {
      console.log('All caches cleared');
      // Reload the page after clearing caches
      window.location.reload();
    }).catch(function(error) {
      console.log('Cache clearing failed:', error);
    });
  } else {
    // If no cache API, just reload
    window.location.reload();
  }
})();
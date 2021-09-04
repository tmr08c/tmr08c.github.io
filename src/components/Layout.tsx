import * as React from "react";

import Footer from "./Footer";
import Header from "./Header";

const Layout: React.SFC = ({ children }) => (
  <>
    <Header />
    <main className="min-h-screen mx-auto px-4 py-8 max-w-2xl sm:px-6 sm:py-12 lg:max-w-3xl lg:py-16 2xl:max-w-4xl xl:px-0">
      {children}
    </main>
    <Footer />
  </>
);

export default Layout;

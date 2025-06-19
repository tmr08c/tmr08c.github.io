import * as React from "react";

import useDarkMode from "use-dark-mode";
import { DarkModeSwitch } from "react-toggle-dark-mode";

function DarkModeToggle() {
  const { value: isDarkMode, toggle: toggle } = useDarkMode(false, {
    classNameDark: "dark",
    classNameLight: "light",
  });

  return (
    <DarkModeSwitch
      checked={isDarkMode}
      onChange={toggle}
      size={18}
      moonColor={"#A78BFA"} // purple-400
      sunColor={"white"}
    />
  );
}

export default DarkModeToggle;

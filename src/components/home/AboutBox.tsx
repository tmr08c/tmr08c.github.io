import * as React from 'react'

interface AboutBoxProps {
  customStyle: React.CSSProperties
  sectionTitle: string
}

const AboutBox: React.SFC<AboutBoxProps> = ({
  customStyle,
  sectionTitle,
  children,
}) => (
  <div
    style={customStyle}
    className="
      py-24 px-8 my-3 mx-4
      lg:py-16 lg:px-16 lg:mx-4
      border-blue-800 border-solid
      border-4
      shadow-inner
    "
  >
    <h2 className="mb-4 text-4xl">{sectionTitle}</h2>
    {children}
  </div>
)

export default AboutBox

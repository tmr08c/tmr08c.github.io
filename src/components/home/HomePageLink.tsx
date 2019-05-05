import * as React from 'react'

interface HomePageLinkProps {
  destination: string
  displayText: string
}

const HomePageLink: React.SFC<HomePageLinkProps> = ({
  destination,
  displayText,
}) => (
  <a
    href={destination}
    target="_blank"
    rel="nofollow noopener noreferrer"
    className="ml-1 border-blue-800 border-b border-dashed hover:border-solid"
  >
    {displayText}
  </a>
)

export default HomePageLink

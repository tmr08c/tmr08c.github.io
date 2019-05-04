import * as React from 'react'

interface TalkProps {
  talk: TroyProgBlog.Talk
}

const Talk: React.SFC<TalkProps> = ({ talk }) => {
  return (
    <div className="mb-4">
      <h3 className="text-2xl mb-1">
        {talk.name}
        <span className="ml-2 text-sm text-gray-600">
          {talk.presentationDate.month}/{talk.presentationDate.year}
        </span>
      </h3>
      <div>{talk.description}</div>
    </div>
  )
}

export default Talk

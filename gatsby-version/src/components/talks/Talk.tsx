import * as React from 'react'

interface TalkProps {
  talk: TroyProgBlog.Talk
}

const Talk: React.SFC<TalkProps> = ({ talk }) => {
  return (
    <div className="mb-4">
      <h3 className="text-2xl mb-1">
        {talk.link ? (
          <a
            href={talk.link}
            target="_blank"
            rel="nofollow noopener noreferrer"
          >
            {talk.name}
          </a>
        ) : (
          <span>{talk.name}</span>
        )}

        <span className="ml-2 text-sm text-gray-600">
          {talk.presentationDate.month}/{talk.presentationDate.year}
        </span>
      </h3>
      <div dangerouslySetInnerHTML={{ __html: talk.description }} />
    </div>
  )
}

export default Talk

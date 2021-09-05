import * as React from "react";

interface TalkProps {
  talk: TroyProgBlog.Talk;
}

const Talk: React.SFC<TalkProps> = ({ talk }) => {
  return (
    <div className="mb-10">
      <h3 className="text-xl mb-4">
        {talk.link ? (
          <a
            href={talk.link}
            target="_blank"
            rel="nofollow noopener noreferrer"
            className="border-solid border-b-4 hover:text-purple-400 duration-300 transition-colors"
          >
            {talk.name}
          </a>
        ) : (
          <span>{talk.name}</span>
        )}

        <span className="ml-1 text-xs text-gray-400 tracking-tighter">
          {talk.presentationDate.month}/{talk.presentationDate.year}
        </span>
      </h3>
      <div
        className="text-gray-800 dark:text-gray-300"
        dangerouslySetInnerHTML={{ __html: talk.description }}
      />
    </div>
  );
};

export default Talk;

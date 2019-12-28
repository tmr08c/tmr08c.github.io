import * as React from "react";

interface TalkProps {
  talk: TroyProgBlog.Talk;
}

const Talk: React.SFC<TalkProps> = ({ talk }) => {
  return (
    <div className="mb-5">
      <h3 className="text-xl mb-2">
        {talk.link ? (
          <a
            href={talk.link}
            target="_blank"
            rel="nofollow noopener noreferrer"
            className="border-red-300 border-solid border-b-4"
          >
            {talk.name}
          </a>
        ) : (
          <span>{talk.name}</span>
        )}

        <span className="ml-1 text-sm text-gray-500 tracking-tighter">
          {talk.presentationDate.month}/{talk.presentationDate.year}
        </span>
      </h3>
      <div
        className="text-gray-800"
        dangerouslySetInnerHTML={{ __html: talk.description }}
      />
    </div>
  );
};

export default Talk;

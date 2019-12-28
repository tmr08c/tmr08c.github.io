import * as React from "react";
import Talk from "./Talk";

interface YearListProps {
  year: string;
  talks: TroyProgBlog.Talk[];
}

const YearList: React.SFC<YearListProps> = ({ year, talks }) => (
  <div key={year} className="mb-8">
    <h2 className="tracking-tighter text-3xl mb-3 text-center">{year}</h2>
    {talks.map(talk => (
      <Talk key={talk.name} talk={talk} />
    ))}
  </div>
);

export default YearList;

declare namespace TroyProgBlog {
  interface Talk {
    description: string;
    link?: string;
    name: string;
    presentationDate: {
      month: number;
      year: number;
    };
  }
}

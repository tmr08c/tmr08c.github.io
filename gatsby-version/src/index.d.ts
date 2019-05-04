declare namespace TroyProgBlog {
  interface TalkList {
    [year: string]: Talk[]
  }

  interface Talk {
    description: string
    name: string
    presentationDate: {
      month: number
      year: number
    }
  }
}

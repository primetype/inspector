module Inspector.Export
    ( export
    ) where

import Foundation
import Foundation.VFS

import Inspector.Monad
import Inspector.Method
import Inspector.Dict

import Inspector.Export.Markdown

export :: Golden method
       => OutputType
       -> Proxy method
       -> FilePath
       -> Metadata
       -> [Dict]
       -> GoldenT ()
export Markdown = exportMarkdown

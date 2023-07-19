# Booklist

## Usage:

```
$ booklist [author|genre|title] <filepath>[.yaml|.json]
> Wrote to <filepath>-[author|title|genre]-new.yaml
```

The file you provide can be written in yaml or json.
  The resulting file will always be in yaml.

## How to structure the file:

Title:
  - author: Hans Wurst
    hasRead: false
    hasBought: true
    isbn: null
    rating: 3.2
    genres:
    - Fantasy
    - Horror
    - Programming
    - Meat

If you'd rather make the entries by Genre or author you can simply switch
  they keys eg.:

Hans Wurst:
  - title: Hans Wurst
    hasRead: false
    hasBought: true
    isbn: null
    rating: 3.2
    genres:
    - Fantasy
    - Horror
    - Programming
    - Meat

Basically like this:

```hs
data BookEntryT f
  = BookEntry
      { isbn      :: f (Maybe T.Text)
      , title     :: f T.Text
      , rating    :: f (Maybe Float)
      , hasRead   :: f Bool
      , hasBought :: f Bool
      , author    :: f T.Text
      , genres    :: f [T.Text]
      }
  deriving (Generic)
```

# License

<a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc-sa/4.0/">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License</a>.
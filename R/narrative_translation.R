######### Narrative for country-specific charts ######### 
## list text objects ----
# define English text variables
text_vars_en <- mget(ls(pattern = "^(txt_|tlm_)"))


## translation function ----
translate_batch_deepl <- function(texts, target_lang = "FR", auth_key) {
  
  # 1. Protect {regn} with custom tags so DeepL skips it
  texts_protected <- map_chr(texts, ~ gsub("\\{regn\\}", "<keep>{regn}</keep>", .x))
  
  # 2. Build API request body
  body_list <- list(
    auth_key = auth_key,
    target_lang = target_lang,
    tag_handling = "xml",
    ignore_tags = "keep"
  )
  body_list <- c(body_list, set_names(as.list(texts_protected), rep("text", length(texts_protected))))
  
  
  # 3. Send request
  res <- POST(
    url = "https://api.deepl.com/v2/translate",  # DeepL Pro URL
    body = body_list,
    encode = "form"
  )
  
  # 4. Parse result
  if (status_code(res) == 200) {
    translated_raw <- map_chr(content(res, "parsed")$translations, "text")
    translated_clean <- gsub("</?keep>", "", translated_raw)
    names(translated_clean) <- names(texts)
    return(translated_clean)
  } else {
    stop("Translation failed: ", content(res, "text"))
  }
}

## wrapper for translating only if needed ----
get_translated_texts <- function(texts, lang = "en", auth_key) {
  lang <- tolower(lang)
  if (lang == "en") {
    return(texts)
  } else {
    return(translate_batch_deepl(texts, toupper(lang), auth_key))
  }
}

## specify your API key ----
deepl_key <- "f5a111d6-e50e-4dce-95e1-57b43246c0df"
# deepl_key <- "1b8a82a1-f811-4743-bb04-593b0c7e3112:fx"  # lauren's free api key


## function to retrieve a specific text string by key and language ----
# Function to return translated texts only if not English
get_text_vars <- function(lang = "en") {
  lang <- tolower(lang)
  if (lang == "en") {
    return(text_vars_en)
  } else {
    return(get_translated_texts(text_vars_en, lang = lang, auth_key = deepl_key))
  }
}

# Use this instead of text_vars_fr/text_vars_es/text_vars_en
text_vars <- get_text_vars(lang = language)  # or "fr", "es"

# Then use:
get_text2 <- function(key, text_vars) {
  text_vars[[key]]
}
## translate texts ----




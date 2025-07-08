# Felinize

Yet another HashCat mask generator.
Creates `.hcmask` files with all mobile phone numbers in specified cities,
according to the [`ranges.csv` metadata provided by libphonenumber][1].

根据 [libphonenumber `ranges.csv`][1] 中提供的电话号段信息，
生成包含某地全部手机号的密码字典 `.hcmask` 文件供 HashCat 使用。


### Dependencies 代码依赖

	cabal update
	cabal install --lib split

	curl -#LO https://github.com/google/libphonenumber/raw/master/metadata/metadata.zip
	unzip -fj metadata.zip metadata/86/ranges.csv


### Usage 使用示例

	./felinize.hs 北京 南京 < ranges.csv > ranges.hcmask
	hashcat -a3 -m22000 hash.txt ranges.hcmask


[1]: https://github.com/google/libphonenumber/blob/master/metadata/metadata.zip

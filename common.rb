class Change < Struct.new(:hash, :text, :rev)
	USE_ORIGINAL = [ # For these, just use the original commit date, not merge
		'778723dd794676d6aa1e24080684590d2790b01d'
	]
	DATE_OVERRIDES = {
		'22 June 2017' => '05 June 2017'
	}
	def data
		opts = text.split(' ')
		opts.shift
		Hash[opts.map do |opt|
			name, val = opt.split('=')
			[name.to_sym, val]
		end]
	end

	def method_missing(name)
		data.fetch(name) { super }
	end

	def merge_time
		merge = if USE_ORIGINAL.include? hash
			hash
		else
			# https://stackoverflow.com/a/8492711
			a = `git rev-list #{hash}..master --ancestry-path`
			b = `git rev-list #{hash}..master --first-parent`
			(a.split("\n") & b.split("\n")).last
		end
		DATE_OVERRIDES[merge] ||
			Time.at(`git rev-list --timestamp #{merge}`.split(' ')[0].to_i)
	end

	def desc
		desc = case mechanism.to_sym
		when :proposal
			"Amended(#{rev}) by proposal #{id} \"#{title.gsub('_', ' ')}\" (#{author}), #{merge_time.strftime('%-d %B %Y')}"
		when :proposal_title
			"Retitled from \"#{from}\" by proposal #{id} \"#{title.gsub('_', ' ')}\" (#{author}), #{merge_time.strftime('%-d %B %Y')}"
		when :cleanup
			"Amended(#{rev}) via Rule 2430 \"Cleanup Time,\" #{merge_time.strftime('%-d %B %Y')}"
		else
			raise "Unknown mechanism #{mechanism}"
		end
		DATE_OVERRIDES.each do |commit_date, real_date|
			desc.gsub! commit_date, real_date
		end
		desc
	end
end

def get_changes(rule)
	`git rev-list --pretty=oneline master "rules/#{rule.name}"`.split("\n")
		.map do |entry|
			hash, *text = entry.split(' ')
			[hash, text.join(' ')]
		end
		.find_all { |entry| entry[1] =~ /^rc / }
		.each_with_index.map do |entry, idx|
			Change.new(*entry, rule.rev + idx + 1)
		end
		.to_a
end

def get_history(rule)
	rule.history + get_changes(rule).map(&:desc)
end

def get_rev(rule)
	get_changes(rule).last&.rev || rule.rev
end
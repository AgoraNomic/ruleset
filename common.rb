class Change < Struct.new(:hash, :text, :rev)
	SPECIAL_CASES = [ # For these, just use the original commit date, not merge
		'778723dd794676d6aa1e24080684590d2790b01d'
	]
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
		merge = if SPECIAL_CASES.include? hash
			hash
		else
			# https://stackoverflow.com/a/8492711
			a = `git rev-list #{hash}..master --ancestry-path`
			b = `git rev-list #{hash}..master --first-parent`
			(a.split("\n") & b.split("\n")).last
		end
		Time.at(`git rev-list --timestamp #{merge}`.split(' ')[0].to_i)
	end

	def desc
		case mechanism.to_sym
		when :cleanup
			"Amended(#{rev}) via Rule 2430 \"Cleanup Time,\" #{merge_time.strftime('%-d %B %Y')}"
		else
			raise 'Unknown mechanism'
		end
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
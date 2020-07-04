class EntityConfig:
    def __init__(self, kind, has_power):
        self.kind = kind
        self.has_power = has_power

class ReportConfig:
    def __init__(self, entity_config, use_stats):
        self.entity_config = entity_config
        self.use_stats = use_stats
